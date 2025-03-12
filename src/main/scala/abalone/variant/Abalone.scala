package strategygames.abalone
package variant

import strategygames.GameFamily

case object Abalone
  extends Variant(
    id = 1,
    key = "abalone",
    name = "Abalone",
    standardInitialPosition = true,
    boardType = Board.Hex5
  ) {

  override def gameFamily: GameFamily = GameFamily.Abalone()

  override def perfIcon: Char = '\ue927'

  override def perfId: Int = 700

  override def baseVariant: Boolean = true // Belgian Daisy initialFen is defined in Abalone default "Variant" file
}