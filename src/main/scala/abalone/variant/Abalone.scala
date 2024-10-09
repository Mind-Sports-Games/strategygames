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

  // TODO: Abalone set
  override def winner(situation: Situation): Option[Player] =
    None // winner is the one who pushed out 6 or prevented opponent to move (which is an extremely rare case)

}
