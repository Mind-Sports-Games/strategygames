package strategygames.go
package variant

import strategygames.go._
import strategygames.GameFamily

case object Go9x9
    extends Variant(
      id = 1,
      key = "go9x9",
      name = "Go 9x9",
      standardInitialPosition = false,
      boardSize = Board.Dim9x9
    )
    with Go9x9Setup {

  def gameFamily: GameFamily = GameFamily.Go()

  def perfIcon: Char = ''
  def perfId: Int    = 500

  override def usesScalaEngine: Boolean = true

}
