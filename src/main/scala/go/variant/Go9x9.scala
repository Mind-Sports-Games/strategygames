package strategygames.go
package variant

import strategygames.go._
import strategygames.{ GameFamily, Player }

case object Go9x9
    extends Variant(
      id = 1,
      key = "go9x9",
      name = "Go 9x9",
      standardInitialPosition = false,
      boardSize = Board.Dim9x9
    ) {

  def gameFamily: GameFamily = GameFamily.Go()

  def perfIcon: Char = ''
  def perfId: Int    = 500

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN("9/9/9/9/9/9/9/9/9[SSSSSSSSSSssssssssss] b - 0 65 65 1")

  override def boardFenFromHandicap(handicap: Int): String = {
    handicap match {
      case 1 => "9/9/6S2/9/9/9/9/9/9"
      case 2 => "9/9/2S3S2/9/9/9/9/9/9"
      case 3 => "9/9/2S3S2/9/9/9/2S6/9/9"
      case 4 => "9/9/2S3S2/9/9/9/2S3S2/9/9"
      case 5 => "9/9/2S3S2/9/4S4/9/2S3S2/9/9"
      case 6 => "9/9/2S1S1S2/9/4S4/9/2S3S2/9/9"
      case 7 => "9/9/2S1S1S2/9/2S1S4/9/2S3S2/9/9"
      case 8 => "9/9/2S1S1S2/9/2S1S4/9/2S1S1S2/9/9"
      case 9 => "9/9/2S1S1S2/9/2S1S1S2/9/2S1S1S2/9/9"
      case _ => "9/9/9/9/9/9/9/9/9"
    }
  }

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
