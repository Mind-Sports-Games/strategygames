package strategygames.fiveinarow
package variant

import strategygames.GameFamily

case object Renju
    extends Variant(
      id = 4,
      key = "renju",
      name = "Renju",
      standardInitialPosition = true,
      boardSize = Board.Dim15x15
    ) {

  def gameFamily: GameFamily = GameFamily.FiveInARow()

  def perfIcon: Char = ''
  def perfId: Int    = 1002

  override def opening: Opening = Opening.Taraguchi10

  // black wins only with exactly five, and an overline is one of black's forbidden moves
  override def winningLine(role: Role, length: Int): Boolean =
    if (role == BlackStone) length == 5 else length >= 5

  override def forbiddenMoves: Boolean = true

  override def canOfferDraw: Boolean = true

}
