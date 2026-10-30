package strategygames.yonkoma
package variant

import strategygames.yonkoma._
import strategygames.GameFamily

case object Yonkoma10x10
    extends Variant(
      id = 1,
      key = "yonkoma10x10",
      name = "Yonkoma 10x10",
      standardInitialPosition = true,
      boardSize = Board.Dim10x10
    ) {

  def gameFamily: GameFamily = GameFamily.Yonkoma()

  def perfIcon: Char = ''
  def perfId: Int    = 1000

  override def baseVariant: Boolean = true

}
