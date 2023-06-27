package strategygames.go
package variant

import strategygames.go._
import strategygames.{ GameFamily, Player }

case object Go13x13
    extends Variant(
      id = 2,
      key = "go13x13",
      name = "Go 13x13",
      standardInitialPosition = true,
      boardSize = Board.Dim13x13
    ) {

  def gameFamily: GameFamily = GameFamily.Go()

  def perfIcon: Char = ''
  def perfId: Int    = 501

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN("13/13/13/13/13/13/13/13/13/13/13/13/13[SSSSSSSSSSssssssssss] b - 0 0 6 1")

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
