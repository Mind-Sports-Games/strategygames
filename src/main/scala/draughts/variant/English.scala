package strategygames.draughts
package variant

import strategygames.Player

import scala.annotation.tailrec
import cats.implicits._

case object English
    extends Variant(
      id = 15,
      gameType = 32,
      key = "english",
      name = "English",
      standardInitialPosition = false,
      boardSize = Board.D64
    ) {

  def perfId: Int = 126

  override def fenVariant = true
  override def aiVariant  = false
  def pieces              = Russian.pieces
  def initialFen          = Russian.initialFen
  def startingPosition    = Russian.startingPosition

  override def playerNames: Map[Player, String]  = Map(P1 -> "Black", P2 -> "White")
  override def playerColors: Map[Player, String] = Map(P1 -> "black", P2 -> "white")

  override def flyingKings: Boolean = false

  // captureDirs is now used for kings within english draughts
  def captureDirs = Standard.captureDirs

  val manCaptureDirsPlayer: Map[Player, Directions] = Portuguese.manCaptureDirsPlayer
  def moveDirsPlayer                                = Standard.moveDirsPlayer
  def moveDirsAll                                   = Standard.moveDirsAll

  override def validMoves(situation: Situation, finalSquare: Boolean = false): Map[Pos, List[Move]] =
    Russian.validMoves(situation, finalSquare)

  // King captures in English are the same as normal 'man' captures in Russian
  override def longRangeCaptures(actor: Actor, finalSquare: Boolean): List[Move] =
    Russian.shortRangeCaptures(actor, finalSquare)

  override def shortRangeCaptures(actor: Actor, finalSquare: Boolean): List[Move] = {
    val buf    = new scala.collection.mutable.ArrayBuffer[Move]
    val player = actor.player

    def walkCaptures(
        walkDir: Direction,
        curBoard: Board,
        curPos: PosMotion,
        firstSquare: Option[PosMotion],
        firstBoard: Option[Board],
        allSquares: List[PosMotion],
        allTaken: List[PosMotion]
    ): Int =
      walkDir._2(curPos).fold(0) { nextPos =>
        curBoard(nextPos) match {
          case Some(captPiece) if captPiece.isNot(player) && !captPiece.isGhost =>
            walkDir._2(nextPos) match {
              case Some(landingPos) if curBoard(landingPos).isEmpty =>
                val takingBoard   = curBoard.takingUnsafe(curPos, landingPos, actor.piece, nextPos, captPiece)
                val promotion     = promotablePos(landingPos, player)
                val boardAfter    =
                  if (promotion) takingBoard.promote(landingPos).getOrElse(takingBoard) else takingBoard
                val promoted      = if (promotion) Some(King) else None
                val newSquares    = landingPos :: allSquares
                val newTaken      = nextPos :: allTaken
                val opposite      = Variant.oppositeDirs(walkDir._1)
                val newDest       = if (firstSquare.isDefined) firstSquare else landingPos.some
                val newBoard      = if (firstBoard.isDefined) firstBoard else boardAfter.some
                val extraCaptures = manCaptureDirsPlayer(player).foldLeft(0) { case (total, captDir) =>
                  if (captDir._1 == opposite) total
                  else {
                    total + walkCaptures(
                      captDir,
                      boardAfter,
                      landingPos,
                      newDest,
                      newBoard,
                      newSquares,
                      newTaken
                    )
                  }
                }
                if (extraCaptures == 0) {
                  val newMove =
                    if (finalSquare)
                      actor.move(landingPos, boardAfter.withoutGhosts, newSquares, newTaken, promoted)
                    else
                      actor.move(
                        firstSquare.getOrElse(landingPos),
                        firstBoard.getOrElse(boardAfter),
                        newSquares,
                        newTaken,
                        promoted
                      )
                  buf += newMove
                }
                extraCaptures + 1
              case _                                                => 0
            }
          case _                                                                => 0
        }
      }

    manCaptureDirsPlayer(player).foreach {
      walkCaptures(_, actor.board, actor.pos, None, None, Nil, Nil)
    }
    buf.toList
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
