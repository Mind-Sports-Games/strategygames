package strategygames.dameo
package variant

import strategygames.dameo._
import strategygames.GameFamily

case object Dameo
    extends Variant(
      id = 1,
      key = "dameo",
      name = "Dameo",
      standardInitialPosition = true,
      boardSize = Board.Dim8x8
    ) {

  def gameFamily: GameFamily = GameFamily.Dameo()

  def perfIcon: Char = ''
  def perfId: Int    = 800

  override def baseVariant: Boolean = true

  // TODO Dameo can write the variant/Variant.scala method code in here (using override)
}
