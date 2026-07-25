package strategygames.go
package variant

import strategygames.go._
import strategygames.GameFamily

case object Go19x19
    extends Variant(
      id = 4,
      key = "go19x19",
      name = "Go 19x19",
      standardInitialPosition = false,
      boardSize = Board.Dim19x19
    )
    with Go19x19Setup {

  def gameFamily: GameFamily = GameFamily.Go()

  def perfIcon: Char = ''
  def perfId: Int    = 502

  override def baseVariant: Boolean = true

}
