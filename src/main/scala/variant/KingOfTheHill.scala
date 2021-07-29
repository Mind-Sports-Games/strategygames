package chess
package variant

case object KingOfTheHill
    extends ChessVariant(
      id = 4,
      key = "kingOfTheHill",
      name = "King of the Hill",
      shortName = "KotH",
      title = "Bring your King to the center to win the game.",
      standardInitialPosition = true,
      boardSize = Board.D64
    ) {

  def pieces = Standard.pieces

  private val center = Set(Pos.D4, Pos.D5, Pos.E4, Pos.E5)

  override def specialEnd(situation: ChessSituation) =
    situation.board.kingPosOf(!situation.color) exists center.contains

  /** You only need a king to be able to win in this variant
    */
  override def opponentHasInsufficientMaterial(situation: ChessSituation) = false
  override def isInsufficientMaterial(board: ChessBoard)                  = false
}
