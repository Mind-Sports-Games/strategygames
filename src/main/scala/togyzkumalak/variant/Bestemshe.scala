package strategygames.togyzkumalak
package variant

import strategygames.togyzkumalak._
import strategygames.{ GameFamily, Player }

case object Bestemshe
    extends Variant(
      id = 2,
      key = "bestemshe",
      name = "Bestemshe",
      standardInitialPosition = false,
      boardSize = Board.Dim5x2
    ) {

  def gameFamily: GameFamily = GameFamily.Togyzkumalak()

  def perfIcon: Char = ''
  def perfId: Int    = 401

  override def baseVariant: Boolean = false

  override def canOfferDraw = false

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN("5S,5S,5S,5S,5S/5S,5S,5S,5S,5S 0 0 S 1")

  override def usesTuzdik = false

  override def specialEnd(situation: Situation) =
    (situation.board.history.score.p1 > 25) ||
      (situation.board.history.score.p2 > 25) ||
      (situation.moves.size == 0)

  // shouldn't happen from starting fen as scores have to always be even so 25=25 is not possible
  override def specialDraw(situation: Situation) =
    situation.board.history.score.p1 == situation.board.history.score.p2

  override def winner(situation: Situation): Option[Player] =
    if (specialEnd(situation) && !specialDraw(situation)) {
      if (situation.board.history.score.p1 > situation.board.history.score.p2)
        Player.fromName("p1")
      else Player.fromName("p2")
    } else None

}
