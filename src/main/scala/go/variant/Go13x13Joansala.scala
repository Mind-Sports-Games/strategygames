package strategygames.go
package variant

import strategygames.go._
import strategygames.GameFamily

case object Go13x13Joansala
    extends Variant(
      id = 6,
      key = "go13x13Joansala",
      name = "Go 13x13 Joansala",
      standardInitialPosition = false,
      boardSize = Board.Dim13x13
    )
    with Go13x13Setup {

  def gameFamily: GameFamily = GameFamily.Go()

  def perfIcon: Char = ''
  def perfId: Int    = 504

}
