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
    format.FEN("9S,9S,9S,9S,9S,9S,9S,9S,9S/9S,9S,9S,9S,9S,9S,9S,9S,9S 0 0 S 1")

  // TODO check legalMoves.size == 0 condition
  override def specialEnd(situation: Situation) =
    (situation.board.history.score.p1 > 81) ||
      (situation.board.history.score.p2 > 81) ||
      (situation.moves.size == 0)

  override def specialDraw(situation: Situation) =
    situation.board.history.score.p1 == situation.board.history.score.p2

  override def winner(situation: Situation): Option[Player] =
    if (specialEnd(situation) && !specialDraw(situation)) {
      if (situation.board.history.score.p1 > situation.board.history.score.p2)
        Player.fromName("p1")
      else Player.fromName("p2")
    } else None

}
