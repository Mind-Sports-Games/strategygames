package strategygames.mancala
package variant

import strategygames.mancala._
import strategygames.GameFamily

case object Oware
    extends Variant(
      id = 1,
      key = "oware",
      name = "Oware",
      standardInitialPosition = true,
      boardSize = Board.Dim6x2
    ) {

  def gameFamily: GameFamily = GameFamily.Mancala()

  def perfIcon: Char = 's'
  def perfId: Int    = 300

  override def baseVariant: Boolean = true

  //cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN("DDDDDD/DDDDDD 0 0 S")


}
