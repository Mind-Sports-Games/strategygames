package strategygames.fiveinarow
package variant

import strategygames.GameFamily

case object Gomoku
    extends Variant(
      id = 1,
      key = "gomoku",
      name = "Gomoku",
      standardInitialPosition = true,
      boardSize = Board.Dim15x15
    ) {

  def gameFamily: GameFamily = GameFamily.FiveInARow()

  def perfIcon: Char = ''
  def perfId: Int    = 1000

  override def baseVariant: Boolean = true

  // an overline is a legal drop for either colour but never wins
  override def winningLine(role: Role, length: Int): Boolean = length == 5

  override def canOfferDraw: Boolean = true

}
