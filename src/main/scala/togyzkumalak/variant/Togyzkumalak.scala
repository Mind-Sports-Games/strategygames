package strategygames.togyzkumalak
package variant

import strategygames.togyzkumalak._
import strategygames.{ GameFamily, Player }

case object Togyzkumalak
    extends Variant(
      id = 1,
      key = "togyzkumalak",
      name = "Togyzkumalak",
      standardInitialPosition = true,
      boardSize = Board.Dim9x2
    ) {

  def gameFamily: GameFamily = GameFamily.Togyzkumalak()

  def perfIcon: Char = ''
  def perfId: Int    = 400

  override def baseVariant: Boolean = true

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN("9S,9S,9S,9S,9S,9S,9S,9S,t/9S,9S,9S,9S,9S,9S,9S,9S,t 0 0 S 1")

  // TODO: implement
  override def specialEnd(situation: Situation) = false

  // TODO: implement
  override def specialDraw(situation: Situation) = false

  // TODO: implement
  override def winner(situation: Situation): Option[Player] = None

}
