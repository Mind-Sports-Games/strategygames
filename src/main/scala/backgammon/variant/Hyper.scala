package strategygames.backgammon
package variant

import strategygames.backgammon._
import strategygames.GameFamily

case object Hyper
    extends Variant(
      id = 4,
      key = "hyper",
      name = "Hyper",
      standardInitialPosition = false,
      boardSize = Board.Dim12x2
    ) {

  def gameFamily: GameFamily = GameFamily.Backgammon()

  def perfIcon: Char = '' // TODO change when available
  def perfId: Int    = 602

  override def numStartingPiecesPerPlayer: Int = 3

  // gammon and backgammon only count when the cube has been doubled in this variant
  override def gammonWin(situation: Situation)     = false
  override def backgammonWin(situation: Situation) = false

  override def baseVariant: Boolean = false

  override def initialFen =
    format.FEN("9,1S,1S,1S/9,1s,1s,1s[] - - w 0 0 1")

}
