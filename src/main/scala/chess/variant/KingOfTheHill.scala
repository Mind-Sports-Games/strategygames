package strategygames.chess.variant

import strategygames.chess._

case object KingOfTheHill
    extends Variant(
      id = 4,
      key = "kingOfTheHill",
      name = "King of the Hill",
      standardInitialPosition = true
    ) {

  def perfId: Int    = 12
  def perfIcon: Char = '('

  def pieces = Standard.pieces

  private val center = Set(Pos.D4, Pos.D5, Pos.E4, Pos.E5)

  override def specialEnd(situation: Situation) =
    situation.board.kingPosOf(!situation.player) exists center.contains

  /** You only need a king to be able to win in this variant
    */
  override def opponentHasInsufficientMaterial(situation: Situation) = false
  override def isInsufficientMaterial(board: Board)                  = false
}
