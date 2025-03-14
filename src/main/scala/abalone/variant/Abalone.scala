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

  def perfIcon: Char = '\ue927'
  def perfId: Int    = 700

  override def baseVariant: Boolean =
    true // Belgian Daisy initialFen is defined in Abalone default "Variant" file
}
