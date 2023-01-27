package strategygames.draughts
package variant

import strategygames.Player

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

  def perfId: Int    = 126
  def perfIcon: Char = ''

  override def fenVariant = true
  def pieces              = Russian.pieces
  def initialFen          = Russian.initialFen
  def startingPosition    = Russian.startingPosition

  override val openingTables = List(OpeningTable.tableACF11ManBallot)

  override def playerNames: Map[Player, String]  = Map(P1 -> "Black", P2 -> "White")
  override def playerColors: Map[Player, String] = Map(P1 -> "black", P2 -> "white")

  override def invertNumericCoords: Boolean = true

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
  ): Board = Pool.finalizeBoard(board, uci, captured, situationBefore, finalSquare)

  def maxDrawingMoves(board: Board): Option[Int] = drawingMoves

  // Only auto draw rule for American is 40 moves with no progress
  private def drawingMoves: Option[Int] = Some(80)

  def updatePositionHashes(
      board: Board,
      move: Move,
      hash: strategygames.draughts.PositionHash
  ): PositionHash = {
    val newHash = Hash(Situation(board, !move.piece.player))
    drawingMoves match {
      case Some(_) =>
        if (move.captures || move.promotes)
          newHash // reset hash on capture or promotion
        else newHash ++ hash
      case _       => newHash
    }
  }

  override def validSide(board: Board, strict: Boolean)(player: Player) =
    Russian.validSide(board, strict)(player)

}
