package strategygames.entropy
package variant

import strategygames.entropy._
import strategygames.GameFamily

case object Entropy
    extends Variant(
      id = 1,
      key = "entropy",
      name = "Entropy",
      standardInitialPosition = true,
      boardSize = Board.Dim7x7
    ) {

  def gameFamily: GameFamily = GameFamily.Entropy()

  def perfIcon: Char = '\uE935'
  def perfId: Int    = 900

  override def baseVariant: Boolean = true

}
