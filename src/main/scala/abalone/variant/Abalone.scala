package strategygames.abalone
package variant

import strategygames.GameFamily
import strategygames.abalone._

case object Abalone
    extends Variant(
      id = 1,
      key = "abalone",
      name = "Abalone",
      standardInitialPosition = true,
      boardSize = Board.Dim9x9
    ) {

  def gameFamily: GameFamily = GameFamily.Abalone()

  def perfIcon: Char = ''
  def perfId: Int    = 700

  override def baseVariant: Boolean = true
}
