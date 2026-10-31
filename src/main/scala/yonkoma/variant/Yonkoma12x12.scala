package strategygames.yonkoma
package variant

import strategygames.yonkoma._
import strategygames.GameFamily

case object Yonkoma12x12
    extends Variant(
      id = 2,
      key = "yonkoma12x12",
      name = "Yonkoma 12x12",
      standardInitialPosition = true,
      boardSize = Board.Dim12x12
    ) {

  def gameFamily: GameFamily = GameFamily.Yonkoma()

  def perfIcon: Char = ''
  def perfId: Int    = 1001

}
