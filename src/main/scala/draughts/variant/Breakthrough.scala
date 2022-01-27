package strategygames.draughts
package variant

import strategygames.Player

import cats.implicits._

case object Breakthrough
    extends Variant(
      id = 9,
      gameType = 96,
      key = "breakthrough",
      name = "Breakthrough",
      shortName = "BT",
      title = "The first player who makes a king wins.",
      standardInitialPosition = true,
      boardSize = Board.D100
    ) {

  def perfId: Int = 117

  def pieces           = Standard.pieces
  def initialFen       = Standard.initialFen
  def startingPosition = Standard.startingPosition

  def captureDirs   = Standard.captureDirs
  def moveDirsPlayer = Standard.moveDirsPlayer
  def moveDirsAll   = Standard.moveDirsAll

  // Win on promotion
  override def specialEnd(situation: Situation) =
    situation.board.kingPosOf(P1).isDefined || situation.board.kingPosOf(P2).isDefined

  override def winner(situation: Situation): Option[Player] =
    if (situation.checkMate) Some(!situation.player)
    else if (situation.board.kingPosOf(P1).isDefined) P1.some
    else if (situation.board.kingPosOf(P2).isDefined) P2.some
    else None

  def maxDrawingMoves(board: Board): Option[Int] = None

  /** No drawing rules
    */
  def updatePositionHashes(board: Board, move: Move, hash: strategygames.draughts.PositionHash): PositionHash =
    Hash(Situation(board, !move.piece.player))

}
