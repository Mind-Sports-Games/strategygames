package strategygames.go
package variant

import strategygames.go._
import strategygames.{ GameFamily, Player }

case object Go19x19
    extends Variant(
      id = 1,
      key = "go19x19",
      name = "Go 19x19",
      standardInitialPosition = true,
      boardSize = Board.Dim19x19
    ) {

  def gameFamily: GameFamily = GameFamily.Go()

  def perfIcon: Char = '' // todo change
  def perfId: Int    = 502

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN("19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19 B - 0 0 1")

  override def specialEnd(situation: Situation) =
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
