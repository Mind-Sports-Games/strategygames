package strategygames.go
package variant

import strategygames.go._
import strategygames.GameFamily

case object Go19x19Scala
    extends Variant(
      id = 7,
      key = "go19x19Scala",
      name = "Go 19x19 Scala",
      standardInitialPosition = false,
      boardSize = Board.Dim19x19
    )
    with Go19x19Setup {

  def gameFamily: GameFamily = GameFamily.Go()

  def perfIcon: Char = ''
  def perfId: Int    = 505

  override def usesScalaEngine: Boolean = true

}
