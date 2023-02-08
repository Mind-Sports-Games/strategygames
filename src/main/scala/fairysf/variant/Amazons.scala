package strategygames.fairysf
package variant

import strategygames.GameFamily

case object Amazons
    extends Variant(
      id = 8,
      key = "amazons",
      name = "Amazons",
      standardInitialPosition = true,
      fairysfName = FairySFName("amazons"),
      boardSize = Board.Dim10x10
    ) {

  def gameFamily: GameFamily = GameFamily.Amazons()

  def perfIcon: Char = ''
  def perfId: Int    = 206

  override def baseVariant: Boolean = true
  override def repetitionEnabled    = false

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN("3q2q3/10/10/q8q/10/10/Q8Q/10/10/3Q2Q3 w - - 0 1")

}
