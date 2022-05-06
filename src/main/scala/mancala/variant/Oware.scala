package strategygames.mancala
package variant

import strategygames.mancala._
import strategygames.{ GameFamily, Player }

case object Oware
    extends Variant(
      id = 1,
      key = "oware",
      name = "Oware",
      standardInitialPosition = true,
      boardSize = Board.Dim6x2
    ) {

  def gameFamily: GameFamily = GameFamily.Mancala()

  def perfIcon: Char = 's'
  def perfId: Int    = 300

  override def baseVariant: Boolean = true

  //cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN("DDDDDD/DDDDDD 0 0 S")

  override def specialEnd(situation: Situation) =
    (situation.board.apiPosition.fen.player1Score > 24) ||
    (situation.board.apiPosition.fen.player2Score > 24) ||
    (situation.board.apiPosition.legalMoves.size == 0) ||
    (situation.board.apiPosition.gameEnd)

  override def specialDraw(situation: Situation) =
    situation.board.apiPosition.fen.gameEndPlayer1Score == situation.board.apiPosition.fen.gameEndPlayer2Score

  // def engineWinner(situation: Situation): Option[Player] = {
  //   if (situation.board.apiPosition.gameOutcome.abs == 1000){
  //     if (situation.board.apiPosition.gameOutcome == 1000) Player.fromName("p1") else Player.fromName("p2")
  //   } else None
  // }
  override def winner(situation: Situation): Option[Player] =
    if (specialEnd(situation) && !specialDraw(situation))
      {if(situation.board.apiPosition.fen.gameEndPlayer1Score > situation.board.apiPosition.fen.gameEndPlayer2Score) Player.fromName("p1") else Player.fromName("p2")}
    else None

}
