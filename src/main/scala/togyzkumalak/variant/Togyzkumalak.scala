package strategygames.togyzkumalak
package variant

import strategygames.togyzkumalak._

case object Togyzkumalak
    extends Variant(
      id = 1,
      key = "togyzkumalak",
      name = "Togyzkumalak",
      standardInitialPosition = true,
      boardSize = Board.Dim9x2
    ) {

  def perfIcon: Char = '›'
  def perfId: Int    = 400

  override def baseVariant: Boolean = true

  override def initialFen =
    format.FEN("9S,9S,9S,9S,9S,9S,9S,9S,9S/9S,9S,9S,9S,9S,9S,9S,9S,9S 0 0 S 1")

}
