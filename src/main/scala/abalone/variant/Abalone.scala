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
      boardSize = Board.Dim9x2
    ) {

  def gameFamily: GameFamily = GameFamily.Abalone()

  def perfIcon: Char = ''
  def perfId: Int    = 700

  override def baseVariant: Boolean = true

  // TODO: Abalone set
  override def initialFen = format.FEN("")

  // TODO: Abalone set
  override def winner(situation: Situation): Option[Player] = None

}
