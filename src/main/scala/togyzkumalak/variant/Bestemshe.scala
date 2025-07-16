package strategygames.togyzkumalak
package variant

import strategygames.togyzkumalak._

case object Bestemshe
    extends Variant(
      id = 2,
      key = "bestemshe",
      name = "Bestemshe",
      standardInitialPosition = false,
      boardSize = Board.Dim5x2
    ) {

  def perfIcon: Char = ''
  def perfId: Int    = 401

  override def baseVariant: Boolean = false

  override def canOfferDraw = false

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN("5S,5S,5S,5S,5S/5S,5S,5S,5S,5S 0 0 S 1")

  override def initialStoneCount = 50

  override def usesTuzdik = false

}
