package strategygames.draughts
package variant

case object FromPosition
    extends Variant(
      id = 3,
      gameType = 99,
      key = "fromPosition",
      name = "From Position",
      standardInitialPosition = false,
      boardSize = Board.D100
    ) {

  def perfId: Int = Standard.perfId

  def pieces           = Standard.pieces
  def initialFen       = Standard.initialFen
  def startingPosition = Standard.startingPosition

  def captureDirs    = Standard.captureDirs
  def moveDirsPlayer = Standard.moveDirsPlayer
  def moveDirsAll    = Standard.moveDirsAll

  def maxDrawingMoves(board: Board): Option[Int] = Standard.maxDrawingMoves(board)
  def updatePositionHashes(
      board: Board,
      move: Move,
      hash: strategygames.draughts.PositionHash
  ): PositionHash =
    Standard.updatePositionHashes(board, move, hash)
}
