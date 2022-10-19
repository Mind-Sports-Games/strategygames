package strategygames.draughts
package variant

import strategygames.Player

import scala.annotation.tailrec
import cats.implicits._

case object Portuguese
    extends Variant(
      id = 14,
      gameType = 31,
      key = "portuguese",
      name = "Portuguese",
      standardInitialPosition = false,
      boardSize = Board.D64
    ) {
  import Variant._
  def perfId: Int = 125
  def perfIcon: Char = 'K'

  override def fenVariant = true
  override def aiVariant  = false
  def pieces              = Russian.pieces
  def initialFen          = Russian.initialFen
  def startingPosition    = Russian.startingPosition

  // captureDirs is now used for kings within portuguese draughts
  def captureDirs = Standard.captureDirs

  // define capture of men as they cannot capture backwards in portuguese draughts (to be used in shortrange capture)
  val manCaptureDirsPlayer: Map[Player, Directions] = Map(
    P1 -> List((UpLeft, _.moveUpLeft), (UpRight, _.moveUpRight)),
    P2 -> List((DownLeft, _.moveDownLeft), (DownRight, _.moveDownRight))
  )
  def moveDirsPlayer                                = Standard.moveDirsPlayer
  def moveDirsAll                                   = Standard.moveDirsAll

  override def getCaptureValue(board: Board, taken: List[Pos]) = taken.foldLeft(0) { (t, p) =>
    t + getCaptureValue(board, p)
  }

  override def getCaptureValue(board: Board, taken: Pos) =
    board(taken) match {
      case Some(piece) if piece.role == King => 101
      case Some(piece) if piece.role == Man  => 100
      case _                                 => 0
    }

  override def validMoves(situation: Situation, finalSquare: Boolean = false): Map[Pos, List[Move]] = {
    var bestLineValue = 0
    var captureMap    = Map[Pos, List[Move]]()
    for (actor <- situation.actors) {
      val capts = if (finalSquare) actor.capturesFinal else actor.captures
      if (capts.nonEmpty) {
        val lineValue = capts.head.taken.fold(0)(getCaptureValue(situation.board, _))
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

  // HACKFIX: prevent maxing out CPU due to extreme recursion in frisian positions like W:WK5:BK2,K4,K7,K8,K9,K10,K11,K13,K15,K16,K18,K19,K20,K21,K22,K24,K27,K29,K30,K31,K32,K33,K35,K36,K38,K40,K41,K42,K43,K44,K47,K49
  // search is aborted after * wet finger * 2000 cache entries - this should be enough for any practical game position, but may lead to incorrect dests in extreme frisian analysis (soit)
  private val maxCache = 2000

  override def shortRangeCaptures(actor: Actor, finalSquare: Boolean): List[Move] = {
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
                        manCaptureDirsPlayer(actor.player).foreach { captDir =>
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

    manCaptureDirsPlayer(actor.player).foreach {
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

  override def finalizeBoard(
      board: Board,
      uci: format.Uci.Move,
      captured: Option[List[Piece]],
      situationBefore: Situation,
      finalSquare: Boolean
  ): Board = {
    val remainingCaptures =
      if (finalSquare) 0 else situationBefore.captureLengthFrom(uci.orig).getOrElse(0) - 1
    if (remainingCaptures > 0) board
    else {
      val p1Actors                                                       = board.actorsOf(Player.P1)
      val p2Actors                                                       = board.actorsOf(Player.P2)
      val p1Kings                                                        = p1Actors.count(_.piece is King)
      val p2Kings                                                        = p2Actors.count(_.piece is King)
      val p1Pieces                                                       = p1Actors.size
      val p2Pieces                                                       = p2Actors.size
      def loneKing(strongPieces: Int, strongKings: Int, weakKing: Actor) =
        strongPieces == 3 && strongKings >= 1 && weakKing.onLongDiagonal && board.piecesOnLongDiagonal == 1
      val p1LoneKing                                                     =
        if (p1Kings == 1 && p1Pieces == 1 && p2Kings >= 1) {
          loneKing(p2Pieces, p2Kings, p1Actors.head)
        } else false
      val p2LoneKing                                                     =
        if (p2Kings == 1 && p2Pieces == 1 && p1Kings >= 1) {
          loneKing(p1Pieces, p1Kings, p2Actors.head)
        } else false
      if (p1LoneKing || p2LoneKing) {
        board updateHistory { h =>
          // "abuse" kingmove counter to count the amount of moves made on the long
          // diagonal by the side with a lone king against 3 (see 7.2.7)
          h.withKingMove(Player(p1LoneKing), None, true)
        } withoutGhosts
      } else board.withoutGhosts
    }
  }

  def maxDrawingMoves(board: Board): Option[Int] = Russian.maxDrawingMoves(board)
  def updatePositionHashes(
      board: Board,
      move: Move,
      hash: strategygames.draughts.PositionHash
  ): PositionHash =
    Russian.updatePositionHashes(board, move, hash)

  override def validSide(board: Board, strict: Boolean)(player: Player) = {
    val roles = board rolesOf player
    (roles.count(_ == Man) > 0 || roles.count(_ == King) > 0) &&
    (!strict || roles.size <= 12) &&
    (!menOnPromotionRank(board, player) || board.ghosts != 0)
  }
}
