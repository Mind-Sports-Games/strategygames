package strategygames.draughts
package variant

import strategygames.{ GameFamily, Player }

import format.FEN

import cats.data.Validated
import cats.implicits._
import scala.annotation.tailrec

// Correctness depends on singletons for each variant ID
abstract class Variant private[variant] (
    val id: Int,
    val gameType: Int,
    val key: String,
    val name: String,
    val standardInitialPosition: Boolean,
    val boardSize: Board.BoardSize
) {

  def pieces: Map[Pos, Piece]
  def initialFen: FEN
  def startPlayer: Player               = P1
  def startingPosition: StartingPosition
  val openingTables: List[OpeningTable] = Nil
  lazy val shortInitialFen: FEN         = FEN(initialFen.value.split(":").take(3).mkString(":"))

  def captureDirs: Directions
  def moveDirsPlayer: Map[Player, Directions]
  def moveDirsAll: Directions

  def standard     = this == Standard
  def frisian      = this == Frisian
  def frysk        = this == Frysk
  def antidraughts = this == Antidraughts
  def breakthrough = this == Breakthrough
  def russian      = this == Russian
  def brazilian    = this == Brazilian
  def pool         = this == Pool
  def portuguese   = this == Portuguese
  def fromPosition = this == FromPosition

  def frisianVariant    = frisian || frysk
  def draughts64Variant = russian || brazilian || pool || portuguese
  def exotic            = !standard

  def baseVariant: Boolean = false
  def fenVariant: Boolean  = false
  def aiVariant: Boolean   = true

  def p1IsBetterVariant: Boolean = false
  def blindModeVariant: Boolean  = false

  def materialImbalanceVariant: Boolean = false

  def perfId: Int
  def perfIcon: Char = 'K'

  def isValidPromotion(promotion: Option[PromotableRole]) = promotion match {
    case None       => true
    case Some(King) => true
    case _          => false
  }

  def getCaptureValue(board: Board, taken: List[Pos]) = taken.length
  def getCaptureValue(board: Board, taken: Pos)       = 1

  def validMoves(situation: Situation, finalSquare: Boolean = false): Map[Pos, List[Move]] = {
    var bestLineValue = 0
    var captureMap    = Map[Pos, List[Move]]()
    for (actor <- situation.actors) {
      val capts = if (finalSquare) actor.capturesFinal else actor.captures
      if (capts.nonEmpty) {
        val lineValue = capts.head.taken.fold(0)(_.length)
        if (lineValue > bestLineValue) {
          bestLineValue = lineValue
          captureMap = Map(actor.pos -> capts)
        } else if (lineValue == bestLineValue)
          captureMap = captureMap + (actor.pos -> capts)
      }
    }

    if (captureMap.nonEmpty) captureMap
    else
      situation.actors
        .collect {
          case actor if actor.noncaptures.nonEmpty =>
            actor.pos -> actor.noncaptures
        }
        .to(Map)
  }

  def validMovesFrom(situation: Situation, pos: Pos, finalSquare: Boolean = false): List[Move] =
    situation.actorAt(pos) match {
      case Some(actor) =>
        if (situation.hasCaptures) actor.getCaptures(finalSquare)
        else actor.noncaptures
      case _           => Nil
    }

  def validMoves(actor: Actor): List[Move] = {
    val captures = actor.captures
    if (captures.nonEmpty)
      captures
    else
      actor.noncaptures
  }

  def move(
      situation: Situation,
      from: Pos,
      to: Pos,
      promotion: Option[PromotableRole],
      finalSquare: Boolean = false,
      forbiddenUci: Option[List[String]] = None,
      captures: Option[List[Pos]] = None,
      partialCaptures: Boolean = false
  ): Validated[String, Move] = {

    // Find the move in the variant specific list of valid moves
    def findMove(from: Pos, to: Pos) = {
      val moves      = validMovesFrom(situation, from, finalSquare)
      val exactMatch = moves.find { m =>
        if (forbiddenUci.fold(false)(_.contains(m.toUci.uci))) false
        else m.dest == to && captures.fold(true)(m.capture.contains)
      }
      if (exactMatch.isEmpty && partialCaptures && captures.isDefined) {
        moves.find { m =>
          if (forbiddenUci.fold(false)(_.contains(m.toUci.uci))) false
          else
            m.capture.isDefined && captures.fold(false) { capts =>
              m.capture.get.endsWith(capts)
            }
        }
      } else exactMatch
    }

    for {
      actor <- situation.board.actors get from toValid "No piece on " + from
      _     <-
        if (actor is situation.player) Validated.valid(actor)
        else Validated.invalid("Not my piece on " + from)
      m1    <- findMove(from, to) toValid "Piece on " + from + " cannot move to " + to
      m2    <- m1 withPromotion promotion toValid "Piece on " + from + " cannot promote to " + promotion
      m3    <-
        if (isValidPromotion(promotion)) Validated.valid(m2)
        else Validated.invalid("Cannot promote to " + promotion + " in this game mode")
    } yield m3

  }

  def promotablePos(pos: PosMotion, player: Player) =
    pos.y == player.fold(boardSize.promotableYP1, boardSize.promotableYP2)

  def maybePromote(m: Move): Option[Move] =
    if (promotablePos(m.after.posAt(m.dest), m.player))
      (m.after promote m.dest) map { b2 =>
        m.copy(after = b2, promotion = Some(King))
      }
    else Some(m)

  // HACKFIX: prevent maxing out CPU due to extreme recursion in frisian positions like W:WK5:BK2,K4,K7,K8,K9,K10,K11,K13,K15,K16,K18,K19,K20,K21,K22,K24,K27,K29,K30,K31,K32,K33,K35,K36,K38,K40,K41,K42,K43,K44,K47,K49
  // search is aborted after * wet finger * 2000 cache entries - this should be enough for any practical game position, but may lead to incorrect dests in extreme frisian analysis (soit)
  private val maxCache = 2000

  def shortRangeCaptures(actor: Actor, finalSquare: Boolean): List[Move] = {
    val buf              = new scala.collection.mutable.ArrayBuffer[Move]
    var bestCaptureValue = 0

    // "transposition table", dramatically reduces calculation time for extreme frisian positions like W:WK50:B3,7,10,12,13,14,17,20,21,23,25,30,32,36,38,39,41,43,K47
    var extraCaptsCache: Option[scala.collection.mutable.LongMap[Int]] = None

    def walkCaptures(
        walkDir: Direction,
        curBoard: Board,
        curPos: PosMotion,
        destPos: Option[PosMotion],
        destBoard: Option[Board],
        allSquares: List[Pos],
        allTaken: List[Pos],
        captureValue: Int
    ): Int =
      if (extraCaptsCache.exists(_.size > maxCache)) 0
      else
        walkDir._2(curPos) match {
          case Some(nextPos) =>
            curBoard(nextPos) match {
              case Some(captPiece) if captPiece.isNot(actor.player) && !captPiece.isGhost =>
                walkDir._2(nextPos) match {
                  case Some(landingPos) if curBoard(landingPos).isEmpty =>
                    val boardAfter       =
                      curBoard.takingUnsafe(curPos, landingPos, actor.piece, nextPos, captPiece)
                    val hash             = if (extraCaptsCache.isDefined) boardAfter.pieces.hashCode() + walkDir._1 else 0
                    val cachedExtraCapts = extraCaptsCache.flatMap(_ get hash)
                    cachedExtraCapts match {
                      case Some(extraCapts) if captureValue + extraCapts < bestCaptureValue =>
                        // no need to calculate lines where we know they will end up too short
                        captureValue + extraCapts
                      case _                                                                =>
                        val newSquares      = landingPos :: allSquares
                        val newTaken        = nextPos :: allTaken
                        val newCaptureValue = captureValue + getCaptureValue(actor.board, nextPos)
                        if (newCaptureValue > bestCaptureValue) {
                          bestCaptureValue = newCaptureValue
                          buf.clear()
                          if (
                            extraCaptsCache.isEmpty && boardAfter.variant.frisianVariant && newTaken.size > 10
                          ) {
                            extraCaptsCache = scala.collection.mutable.LongMap.empty[Int].some
                          }
                        }
                        if (newCaptureValue == bestCaptureValue) {
                          if (finalSquare)
                            buf += actor.move(landingPos, boardAfter.withoutGhosts, newSquares, newTaken)
                          else
                            buf += actor.move(
                              destPos.getOrElse(landingPos),
                              destBoard.getOrElse(boardAfter),
                              newSquares,
                              newTaken
                            )
                        }
                        val opposite        = Variant.oppositeDirs(walkDir._1)
                        val newDest         = if (destPos.isDefined) destPos else landingPos.some
                        val newBoard        = if (destBoard.isDefined) destBoard else boardAfter.some
                        var maxExtraCapts   = 0
                        captureDirs.foreach { captDir =>
                          if (captDir._1 != opposite) {
                            val extraCapts = walkCaptures(
                              captDir,
                              boardAfter,
                              landingPos,
                              newDest,
                              newBoard,
                              newSquares,
                              newTaken,
                              newCaptureValue
                            ) - newCaptureValue
                            if (extraCapts > maxExtraCapts)
                              maxExtraCapts = extraCapts
                          }
                        }
                        extraCaptsCache.foreach { cache =>
                          if (cachedExtraCapts.isEmpty)
                            cache += (hash, maxExtraCapts)
                        }
                        newCaptureValue + maxExtraCapts
                    }
                  case _                                                => captureValue
                }
              case _                                                                      => captureValue
            }
          case _             => captureValue
        }

    captureDirs.foreach {
      walkCaptures(_, actor.board, actor.pos, None, None, Nil, Nil, 0)
    }

    if (extraCaptsCache.exists(_.size > maxCache)) {
      // TODO: warning
      // logger.warn(s"shortRangeCaptures($finalSquare) aborted with ${extraCaptsCache.get.size} entries for ${actor.piece} at ${actor.pos.shortKey} on ${draughts.format.Forsyth.exportBoard(actor.board)}")
    }

    buf.flatMap { m =>
      if (finalSquare || m.capture.exists(_.length == 1)) maybePromote(m)
      else m.some
    } toList
  }

  def longRangeCaptures(actor: Actor, finalSquare: Boolean): List[Move] = {
    val buf              = new scala.collection.mutable.ArrayBuffer[Move]
    var bestCaptureValue = 0

    // "transposition table", dramatically reduces calculation time for extreme frisian positions like W:WK50:B3,7,10,12,13,14,17,20,21,23,25,30,32,36,38,39,41,43,K47
    var extraCaptsCache: Option[scala.collection.mutable.LongMap[Int]] = None

    @tailrec
    def walkUntilCapture(
        walkDir: Direction,
        curBoard: Board,
        curPos: PosMotion,
        destPos: Option[PosMotion],
        destBoard: Option[Board],
        allSquares: List[Pos],
        allTaken: List[Pos],
        captureValue: Int
    ): Int =
      if (extraCaptsCache.exists(_.size > maxCache)) 0
      else
        walkDir._2(curPos) match {
          case Some(nextPos) =>
            curBoard(nextPos) match {
              case None                                                                   =>
                walkUntilCapture(
                  walkDir,
                  curBoard.moveUnsafe(curPos, nextPos, actor.piece),
                  nextPos,
                  destPos,
                  destBoard,
                  allSquares,
                  allTaken,
                  captureValue
                )
              case Some(captPiece) if captPiece.isNot(actor.player) && !captPiece.isGhost =>
                walkDir._2(nextPos) match {
                  case Some(landingPos) if curBoard(landingPos).isEmpty =>
                    val boardAfter =
                      curBoard.takingUnsafe(curPos, landingPos, actor.piece, nextPos, captPiece)
                    walkAfterCapture(
                      walkDir,
                      boardAfter,
                      landingPos,
                      destPos,
                      destBoard,
                      allSquares,
                      nextPos :: allTaken,
                      captureValue + getCaptureValue(actor.board, nextPos)
                    )
                  case _                                                => captureValue
                }
              case _                                                                      => captureValue
            }
          case _             => captureValue
        }

    def walkAfterCapture(
        walkDir: Direction,
        curBoard: Board,
        curPos: PosMotion,
        destPos: Option[PosMotion],
        destBoard: Option[Board],
        allSquares: List[Pos],
        newTaken: List[Pos],
        newCaptureValue: Int
    ): Int = {
      val hash             = if (extraCaptsCache.isDefined) curBoard.pieces.hashCode() + walkDir._1 else 0
      val cachedExtraCapts = extraCaptsCache.flatMap(_ get hash)
      cachedExtraCapts match {
        case Some(extraCapts) if newCaptureValue + extraCapts < bestCaptureValue =>
          // no need to calculate lines where we know they will end up too short
          newCaptureValue + extraCapts
        case _                                                                   =>
          val newSquares    = curPos :: allSquares
          if (newCaptureValue > bestCaptureValue) {
            bestCaptureValue = newCaptureValue
            buf.clear()
            if (extraCaptsCache.isEmpty && curBoard.variant.frisianVariant && newTaken.size > 10) {
              extraCaptsCache = scala.collection.mutable.LongMap.empty[Int].some
            }
          }
          if (newCaptureValue == bestCaptureValue) {
            if (finalSquare)
              buf += actor.move(curPos, curBoard.withoutGhosts, newSquares, newTaken)
            else
              buf += actor.move(
                destPos.getOrElse(curPos),
                destBoard.getOrElse(curBoard),
                newSquares,
                newTaken
              )
          }
          val opposite      = Variant.oppositeDirs(walkDir._1)
          val newDest       = if (destPos.isDefined) destPos else curPos.some
          val newBoard      = if (destBoard.isDefined) destBoard else curBoard.some
          var maxExtraCapts = 0
          captureDirs.foreach { captDir =>
            if (captDir._1 != opposite) {
              val extraCapture = walkUntilCapture(
                captDir,
                curBoard,
                curPos,
                newDest,
                newBoard,
                newSquares,
                newTaken,
                newCaptureValue
              ) - newCaptureValue
              if (extraCapture > maxExtraCapts)
                maxExtraCapts = extraCapture
            }
          }
          walkDir._2(curPos) match {
            case Some(nextPos) if curBoard(nextPos).isEmpty =>
              val extraCapts = walkAfterCapture(
                walkDir,
                curBoard.moveUnsafe(curPos, nextPos, actor.piece),
                nextPos,
                destPos,
                destBoard,
                allSquares,
                newTaken,
                newCaptureValue
              ) - newCaptureValue
              if (extraCapts > maxExtraCapts)
                maxExtraCapts = extraCapts
            case _                                          =>
          }
          extraCaptsCache.foreach { cache =>
            if (cachedExtraCapts.isEmpty)
              cache += (hash, maxExtraCapts)
          }
          newCaptureValue + maxExtraCapts
      }
    }

    captureDirs.foreach {
      walkUntilCapture(_, actor.board, actor.pos, None, None, Nil, Nil, 0)
    }

    if (extraCaptsCache.exists(_.size > maxCache)) {
      // TODO: warning?
      // logger.warn(s"longRangeCaptures($finalSquare) aborted with ${extraCaptsCache.get.size} entries for ${actor.piece} at ${actor.pos.shortKey} on ${draughts.format.Forsyth.exportBoard(actor.board)}")
    }

    buf.toList
  }

  def checkmate(situation: Situation) = situation.validMoves.isEmpty

  // In most variants, the winner is the last player to have played and there is a possibility of either a traditional
  // checkmate or a variant end condition
  def winner(situation: Situation): Option[Player] =
    if (situation.checkMate || specialEnd(situation)) Some(!situation.player) else None

  def specialEnd(situation: Situation)  = false
  def specialDraw(situation: Situation) = false

  // Some variants have an extra effect on the board on a move. For example, in Atomic, some
  // pieces surrounding a capture explode
  def hasMoveEffects = false

  /** Applies a variant specific effect to the move. This helps decide whether a king is endangered by a move,
    * for example
    */
  def addVariantEffect(move: Move): Move = move

  /** Returns the amount of moves until a draw is reached given the material on the board.
    */
  def maxDrawingMoves(board: Board): Option[Int]

  def updatePositionHashes(board: Board, move: Move, hash: strategygames.draughts.PositionHash): PositionHash

  /** Once a move has been decided upon from the available legal moves, the board is finalized This removes
    * any reaining ghostpieces if the capture sequence has ended
    */
  def finalizeBoard(
      board: Board,
      uci: format.Uci.Move,
      captured: Option[List[Piece]],
      situationBefore: Situation,
      finalSquare: Boolean
  ): Board = {
    val remainingCaptures =
      if (finalSquare) 0 else situationBefore.captureLengthFrom(uci.orig).getOrElse(0) - 1
    if (remainingCaptures > 0) board
    else board.withoutGhosts
  }

  protected def menOnPromotionRank(board: Board, player: Player) = {
    board.pieces.exists {
      case (pos, Piece(c, r)) if c == player && r == Man && promotablePos(board.posAt(pos), player) => true
      case _                                                                                        => false
    }
  }

  /** Checks board for valid game position
    */
  protected def validSide(board: Board, strict: Boolean)(player: Player) = {
    val roles = board rolesOf player
    (roles.count(_ == Man) > 0 || roles.count(_ == King) > 0) &&
    (!strict || roles.size <= 20) &&
    (!menOnPromotionRank(board, player) || board.ghosts != 0)
  }

  def valid(board: Board, strict: Boolean) = Player.all forall validSide(board, strict) _

  val roles                            = List(Man, King)
  lazy val rolesByPdn: Map[Char, Role] = roles.map { r => (r.pdn, r) }.to(Map)

  override def toString = s"Variant($name)"

  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  override def hashCode: Int = id

  def gameFamily: GameFamily = GameFamily.Draughts()
}

object Variant {

  val UpLeft    = 1
  val UpRight   = 2
  val DownLeft  = 3
  val DownRight = 4
  val Up        = 5
  val Down      = 6
  val Left      = 7
  val Right     = 8

  val oppositeDirs: Array[Int] = Array(
    0,
    DownRight,
    DownLeft,
    UpRight,
    UpLeft,
    Down,
    Up,
    Right,
    Left
  )

  val all   = List(
    Standard,
    Frisian,
    Frysk,
    Antidraughts,
    Breakthrough,
    Russian,
    Brazilian,
    Pool,
    Portuguese,
    FromPosition
  )
  val byId  = all map { v => (v.id, v) } toMap
  val byKey = all map { v => (v.key, v) } toMap

  val allVariants = all.filter(FromPosition !=)

  val default = Standard

  def apply(id: Int): Option[Variant]     = byId get id
  def apply(key: String): Option[Variant] = byKey get key
  def orDefault(id: Int): Variant         = apply(id) | default
  def orDefault(key: String): Variant     = apply(key) | default

  def byName(name: String): Option[Variant] =
    all find (_.name.toLowerCase == name.toLowerCase)

  def byGameType(gameType: Int): Option[Variant] =
    all find (_.gameType == gameType)

  def exists(id: Int): Boolean = byId contains id

  val openingSensibleVariants: Set[Variant] = Set(
    strategygames.draughts.variant.Standard,
    strategygames.draughts.variant.Frisian,
    strategygames.draughts.variant.Breakthrough,
    strategygames.draughts.variant.Russian,
    strategygames.draughts.variant.Brazilian,
    strategygames.draughts.variant.Pool,
    strategygames.draughts.variant.Portuguese
  )

  val divisionSensibleVariants: Set[Variant] = Set(
    strategygames.draughts.variant.Standard,
    strategygames.draughts.variant.Frisian,
    strategygames.draughts.variant.Antidraughts,
    strategygames.draughts.variant.Breakthrough,
    strategygames.draughts.variant.Russian,
    strategygames.draughts.variant.Brazilian,
    strategygames.draughts.variant.Pool,
    strategygames.draughts.variant.Portuguese,
    strategygames.draughts.variant.FromPosition
  )

  private[variant] def symmetricFourRank(
      rank: IndexedSeq[Role],
      boardSize: Board.BoardSize
  ): Map[Pos, Piece] = {
    (for (y <- Seq(1, 2, 3, 4, 7, 8, 9, 10); x <- 1 to 5) yield {
      boardSize.pos.posAt(x, y) map { pos =>
        (
          pos,
          y match {
            case 1  => Piece(P2, rank(x - 1))
            case 2  => Piece(P2, rank(x - 1))
            case 3  => Piece(P2, rank(x - 1))
            case 4  => Piece(P2, rank(x - 1))
            case 7  => Piece(P1, rank(x - 1))
            case 8  => Piece(P1, rank(x - 1))
            case 9  => Piece(P1, rank(x - 1))
            case 10 => Piece(P1, rank(x - 1))
          }
        )
      }
    }).flatten.toMap
  }

  private[variant] def symmetricThreeRank(
      rank: IndexedSeq[Role],
      boardSize: Board.BoardSize
  ): Map[Pos, Piece] = {
    (for (y <- Seq(1, 2, 3, 6, 7, 8); x <- 1 to 4) yield {
      boardSize.pos.posAt(x, y) map { pos =>
        (
          pos,
          y match {
            case 1 => Piece(P2, rank(x - 1))
            case 2 => Piece(P2, rank(x - 1))
            case 3 => Piece(P2, rank(x - 1))
            case 6 => Piece(P1, rank(x - 1))
            case 7 => Piece(P1, rank(x - 1))
            case 8 => Piece(P1, rank(x - 1))
          }
        )
      }
    }).flatten.toMap
  }

  private[variant] def symmetricBackrank(
      rank: IndexedSeq[Role],
      boardSize: Board.BoardSize
  ): Map[Pos, Piece] = {
    (for (y <- Seq(1, 10); x <- 1 to 5) yield {
      boardSize.pos.posAt(x, y) map { pos =>
        (
          pos,
          y match {
            case 1  => Piece(P2, rank(x - 1))
            case 10 => Piece(P1, rank(x - 1))
          }
        )
      }
    }).flatten.toMap
  }

}
