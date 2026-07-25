package strategygames.go
package variant

import strategygames.go._
import strategygames.GameFamily

case object Go13x13
    extends Variant(
      id = 2,
      key = "go13x13",
      name = "Go 13x13",
      standardInitialPosition = false,
      boardSize = Board.Dim13x13
    )
    with Go13x13Setup {

  def gameFamily: GameFamily = GameFamily.Go()

  def perfIcon: Char = ''
  def perfId: Int    = 501

  override def usesScalaEngine: Boolean = true

}
