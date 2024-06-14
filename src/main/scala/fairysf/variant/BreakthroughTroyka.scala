package strategygames.fairysf
package variant

import strategygames.GameFamily

case object BreakthroughTroyka
    extends Variant(
      id = 9, // within module fairySF
      key = "breakthroughtroyka",
      name = "Breakthrough",
      standardInitialPosition = true,
      fairysfName = FairySFName("breakthrough"),
      boardSize = Board.Dim8x8
    ) {

  def gameFamily: GameFamily = GameFamily.Breakthrough()

  // linked to a fontFile (received from Alice + put it in LILA in several (2?) places)
  def perfIcon: Char                      = 'a'
  // just need to be unique accross all variants (grep module for perfId to check it does not exist)
  def perfId: Int                         = 208
  override def canOfferDraw               = false
  override def baseVariant: Boolean       = true
  override def repetitionEnabled: Boolean = false
  override def useFairyOptionalGameEnd    = true
  override def p1IsBetterVariant: Boolean = true

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN("pppppppp/pppppppp/8/8/8/8/PPPPPPPP/PPPPPPPP w - - 0 1")

}
