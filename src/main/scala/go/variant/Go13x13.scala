package strategygames.go
package variant

import strategygames.go._
import strategygames.{ GameFamily, Player }

case object Go13x13
    extends Variant(
      id = 2,
      key = "go13x13",
      name = "Go 13x13",
      standardInitialPosition = false,
      boardSize = Board.Dim13x13
    ) {

  def gameFamily: GameFamily = GameFamily.Go()

  def perfIcon: Char = ''
  def perfId: Int    = 501

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN("13/13/13/13/13/13/13/13/13/13/13/13/13[SSSSSSSSSSssssssssss] b - 0 75 0 0 75 0 1")

  override def boardFenFromHandicap(handicap: Int): String = {
    handicap match {
      case 1           => "13/13/13/3S9/13/13/13/13/13/13/13/13/13"
      case 2           => "13/13/13/9S3/13/13/13/13/13/3S9/13/13/13"
      case 3           => "13/13/13/9S3/13/13/13/13/13/3S5S3/13/13/13"
      case 4           => "13/13/13/3S5S3/13/13/13/13/13/3S5S3/13/13/13"
      case 5           => "13/13/13/3S5S3/13/13/6S6/13/13/3S5S3/13/13/13"
      case 6           => "13/13/13/3S5S3/13/13/3S5S3/13/13/3S5S3/13/13/13"
      case 7           => "13/13/13/3S5S3/13/13/3S2S2S3/13/13/3S5S3/13/13/13"
      case 8           => "13/13/13/3S2S2S3/13/13/3S2S6/13/13/3S2S2S3/13/13/13"
      case 9           => "13/13/13/3S2S2S3/13/13/3S2S2S3/13/13/3S2S2S3/13/13/13"
      case 10          => "13/6S6/13/3S2S2S3/13/13/3S2S2S3/13/13/3S2S2S3/13/13/13"
      case 11          => "13/6S6/13/3S2S2S3/13/13/3S2S2S3/13/13/3S2S2S3/13/6S6/13"
      case 12          => "13/6S6/13/3S2S2S3/13/13/1S1S2S2S3/13/13/3S2S2S3/13/6S6/13"
      case 13          => "13/6S6/13/3S2S2S3/13/13/1S1S2S2S1S1/13/13/3S2S2S3/13/6S6/13"
      case 14          => "13/1S4S6/13/3S2S2S3/13/13/1S1S2S2S1S1/13/13/3S2S2S3/13/6S6/13"
      case 15          => "13/1S4S4S1/13/3S2S2S3/13/13/1S1S2S2S1S1/13/13/3S2S2S3/13/6S6/13"
      case 16          => "13/1S4S4S1/13/3S2S2S3/13/13/1S1S2S2S1S1/13/13/3S2S2S3/13/6S4S1/13"
      case 17          => "13/1S4S4S1/13/3S2S2S3/13/13/1S1S2S2S1S1/13/13/3S2S2S3/13/1S4S4S1/13"
      case 18          => "13/1S1S2S4S1/13/3S2S2S3/13/13/1S1S2S2S1S1/13/13/3S2S2S3/13/1S4S4S1/13"
      case 19          => "13/1S1S2S2S1S1/13/3S2S2S3/13/13/1S1S2S2S1S1/13/13/3S2S2S3/13/1S4S4S1/13"
      case 20          => "13/1S1S2S2S1S1/13/3S2S2S1S1/13/13/1S1S2S2S1S1/13/13/3S2S2S3/13/1S4S4S1/13"
      case 21          => "13/1S1S2S2S1S1/13/3S2S2S1S1/13/13/1S1S2S2S1S1/13/13/3S2S2S1S1/13/1S4S4S1/13"
      case 22          => "13/1S1S2S2S1S1/13/3S2S2S1S1/13/13/1S1S2S2S1S1/13/13/3S2S2S1S1/13/1S4S2S1S1/13"
      case 23          => "13/1S1S2S2S1S1/13/3S2S2S1S1/13/13/1S1S2S2S1S1/13/13/3S2S2S1S1/13/1S1S2S2S1S1/13"
      case 24          => "13/1S1S2S2S1S1/13/3S2S2S1S1/13/13/1S1S2S2S1S1/13/13/1S1S2S2S1S1/13/1S1S2S2S1S1/13"
      case 25          => "13/1S1S2S2S1S1/13/1S1S2S2S1S1/13/13/1S1S2S2S1S1/13/13/1S1S2S2S1S1/13/1S1S2S2S1S1/13"
      case x if x > 25 =>
        "13/1S1S2S2S1S1/13/1S1S2S2S1S1/13/13/1S1S2S2S1S1/13/13/1S1S2S2S1S1/13/1S1S2S2S1S1/13"
      case _           => "13/13/13/13/13/13/13/13/13/13/13/13/13"
    }
  }

  override def specialEnd(situation: Situation) =
    (situation.board.apiPosition.legalActions.size == 0) ||
      (situation.board.apiPosition.gameEnd)

  override def specialDraw(situation: Situation) =
    (situation.board.apiPosition.fen.player1Score == situation.board.apiPosition.fen.player2Score) ||
      situation.board.apiPosition.isRepetition

  override def winner(situation: Situation): Option[Player] =
    if (specialEnd(situation) && !specialDraw(situation)) {
      if (situation.board.apiPosition.fen.player1Score > situation.board.apiPosition.fen.player2Score)
        Player.fromName("p1")
      else Player.fromName("p2")
    } else None

}
