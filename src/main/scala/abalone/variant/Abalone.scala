package strategygames.abalone
package variant

import strategygames.abalone._
import strategygames.{ GameFamily, Player }

case object Abalone
    extends Variant(
      id = 1,
      key = "abalone",
      name = "Abalone",
      standardInitialPosition = true,
      boardSize = Board.Dim9x9
    ) {

  def gameFamily: GameFamily = GameFamily.Abalone()

  def perfIcon: Char = ''
  def perfId: Int    = 700

  override def baseVariant: Boolean = true

  // pieces, scoreP1, scoreP2, turn, halfMovesSinceLastCapture (triggering condition could be when == 100 && total moves > 50 ? => draw), total moves
  override def initialFen = format.FEN("pp1PP/pppPPP/1pp1PP1/8/9/8/1PP1pp1/PPPppp/PP1pp 0 0 b 0 0")

  // TODO: Abalone set
  override def winner(situation: Situation): Option[Player] =
    None // winner is the one who pushed out 6 or prevented opponent to move (which is an extremely rare case)

}
