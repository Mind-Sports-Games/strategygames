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
  def perfIcon: Char = ''

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
  ): Board = Brazilian.finalizeBoard(board, uci, captured, situationBefore, finalSquare)

  def maxDrawingMoves(board: Board): Option[Int] =
    drawingMoves(board, none).map(_._1)

  // (drawingMoves, movedOnToLongDiag)
  private def drawingMoves(board: Board, move: Option[Move]): Option[(Int, Boolean)] = {
    val p1Actors       = board.nonGhostActorsForPlayer(Player.P1)
    val p2Actors       = board.nonGhostActorsForPlayer(Player.P2)
    val p1Kings        = p1Actors.count(_.piece is King)
    val p2Kings        = p2Actors.count(_.piece is King)
    val totalPieces    = p1Actors.size + p2Actors.size
    val p1ActorsBefore = move.map(_.before.nonGhostActorsForPlayer(Player.P1)).getOrElse(Seq.empty)
    val p2ActorsBefore = move.map(_.before.nonGhostActorsForPlayer(Player.P2)).getOrElse(Seq.empty)

    def actorsOnLongDiag(actors: Seq[Actor]): Int = actors.map(_.onLongDiagonal).filter(true.==).size

    def longDiagCheck(actors: Seq[Actor], beforeActors: Seq[Actor]): Boolean =
      actorsOnLongDiag(actors) > actorsOnLongDiag(beforeActors) && actorsOnLongDiag(beforeActors) == 0

    def playerOnLongDiagonalWithThreePieces(playerActors: Seq[Actor]): Boolean =
      playerActors.size == 3 && actorsOnLongDiag(playerActors) > 0

    def playerOnLongDiagonal: Option[Player] =
      if (playerOnLongDiagonalWithThreePieces(p1Actors)) Some(Player.P1)
      else if (playerOnLongDiagonalWithThreePieces(p2Actors)) Some(Player.P2)
      else None

    if (p1Kings == 1 && p2Kings == 3 && totalPieces == 4 && playerOnLongDiagonal == Some(Player.P2))
      Some(24, longDiagCheck(p2Actors, p2ActorsBefore)) // 3.2.b
    else if (p1Kings == 3 && p2Kings == 1 && totalPieces == 4 && playerOnLongDiagonal == Some(Player.P1))
      Some(24, longDiagCheck(p1Actors, p1ActorsBefore)) // 3.2.b
    else Some(40, false)                                // 3.2.a
  }

  /** Update position hashes for Portuguese drawing rules
    * (http://www.fpdamas.pt/wp-content/uploads/2021/01/1Regras_Damas_Cla%CC%81ssicas_.pdf): 3.2.a: 20
    * consecutive moves per player without change in state (captures or promotion) 3.2.b: 12 moves per player
    * from the point that the board has 3 Kings + 0 Men vs 1 King + 0 Men and the player with 3 kings has 1 of
    * them on the long diagonal. 3.2.c: 3 fold repetition.
    */
  def updatePositionHashes(
      board: Board,
      move: Move,
      hash: strategygames.draughts.PositionHash
  ): PositionHash = {
    val newHash = Hash(Situation(board, !move.piece.player))
    drawingMoves(board, Some(move)) match {
      case Some((_, movedOnToLongDiag)) =>
        if (move.captures || move.promotes || movedOnToLongDiag)
          newHash // 3.2.a, 3.2.b reset hash on capture or promotion or when king moves onto long diag. Doesnt handle the case when player leaves long diag but in other draughts comments this is described as 'dumb' so isnt handled there either
        else newHash ++ hash
      case _                            => newHash
    }
  }

  override def validSide(board: Board, strict: Boolean)(player: Player) =
    Brazilian.validSide(board, strict)(player)

}
