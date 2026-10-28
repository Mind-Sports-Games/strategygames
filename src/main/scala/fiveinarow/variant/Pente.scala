package strategygames.fiveinarow
package variant

import strategygames.GameFamily

case object Pente
    extends Variant(
      id = 2,
      key = "pente",
      name = "Pente",
      standardInitialPosition = true,
      boardSize = Board.Dim19x19
    ) {

  def gameFamily: GameFamily = GameFamily.FiveInARow()

  def perfIcon: Char = ''
  def perfId: Int    = 1001

  override def captureTarget: Option[Int] = Some(10)

}
