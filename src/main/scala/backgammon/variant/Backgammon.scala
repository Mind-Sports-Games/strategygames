package strategygames.backgammon
package variant

import strategygames.backgammon._
import strategygames.{ GameFamily, Player }

case object Backgammon
    extends Variant(
      id = 1,
      key = "backgammon",
      name = "Backgammon",
      standardInitialPosition = true,
      boardSize = Board.Dim12x2
    ) {

  def gameFamily: GameFamily = GameFamily.Backgammon()

  def perfIcon: Char = ''
  def perfId: Int    = 600

  override def baseVariant: Boolean = true

  override def initialFen =
    format.FEN("5S,3,3s,1,5s,4,2S/5s,3,3S,1,5S,4,2s[] - - w 0 0 1")

  override def specialEnd(situation: Situation) =
    (situation.board.history.score.p1 == 15) ||
      (situation.board.history.score.p2 == 15)

  override def winner(situation: Situation): Option[Player] =
    if (specialEnd(situation)) {
      if (situation.board.history.score.p1 > situation.board.history.score.p2)
        Player.fromName("p1")
      else Player.fromName("p2")
    } else None

}
