package strategygames.go
package variant

import strategygames.go._
import strategygames.GameFamily

case object Go9x9Scala
    extends Variant(
      id = 5,
      key = "go9x9Scala",
      name = "Go 9x9 Scala",
      standardInitialPosition = false,
      boardSize = Board.Dim9x9
    )
    with Go9x9Setup {

  def gameFamily: GameFamily = GameFamily.Go()

  def perfIcon: Char = ''
  def perfId: Int    = 503

  override def usesScalaEngine: Boolean = true

}
