package strategygames.oware
package variant

import strategygames.oware._
import strategygames.GameFamily

case object Oware
    extends Variant(
      id = 1,
      key = "oware",
      name = "Oware",
      standardInitialPosition = true,
      boardSize = Board.Dim6x2
    ) {

  def gameFamily: GameFamily = GameFamily.Oware()

  def perfIcon: Char = 's'
  def perfId: Int    = 300

  override def baseVariant: Boolean = true

  //cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN("eeeeee/eeeeee[-] w 0 1")


}
