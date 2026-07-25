package strategygames.go
package variant

import strategygames.go._
import strategygames.GameFamily

case object Go13x13Scala
    extends Variant(
      id = 6,
      key = "go13x13Scala",
      name = "Go 13x13 Scala",
      standardInitialPosition = false,
      boardSize = Board.Dim13x13
    )
    with Go13x13Setup {

  def gameFamily: GameFamily = GameFamily.Go()

  def perfIcon: Char = ''
  def perfId: Int    = 504

  override def usesScalaEngine: Boolean = true

}
