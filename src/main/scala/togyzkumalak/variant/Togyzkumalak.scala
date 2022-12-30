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
      boardSize = Board.Dim6x2
    ) {

  def gameFamily: GameFamily = GameFamily.Togyzkumalak()

  def perfIcon: Char = ''
  def perfId: Int    = 400

  override def baseVariant: Boolean = true

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN("4S,4S,4S,4S,4S,4S/4S,4S,4S,4S,4S,4S 0 0 S")

  override def specialEnd(situation: Situation) =
    (situation.board.apiPosition.fen.player1Score > 24) ||
      (situation.board.apiPosition.fen.player2Score > 24) ||
      (situation.board.apiPosition.legalMoves.size == 0) ||
      (situation.board.apiPosition.gameEnd)

  override def specialDraw(situation: Situation) =
    situation.board.apiPosition.fen.player1Score == situation.board.apiPosition.fen.player2Score

  override def winner(situation: Situation): Option[Player] =
    if (specialEnd(situation) && !specialDraw(situation)) {
      if (situation.board.apiPosition.fen.player1Score > situation.board.apiPosition.fen.player2Score)
        Player.fromName("p1")
      else Player.fromName("p2")
    } else None

}
