package strategygames.go
package variant

import strategygames.go._
import strategygames.{ GameFamily, Player }

case object Go19x19
    extends Variant(
      id = 4,
      key = "go19x19",
      name = "Go 19x19",
      standardInitialPosition = false,
      boardSize = Board.Dim19x19
    ) {

  def gameFamily: GameFamily = GameFamily.Go()

  def perfIcon: Char = ''
  def perfId: Int    = 502

  override def baseVariant: Boolean = true

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN(
      "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19[SSSSSSSSSSssssssssss] b - 0 75 0 0 75 0 1"
    )

  override def boardFenFromHandicap(handicap: Int): String = {
    handicap match {
      case 1           => "19/19/19/3S15/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19"
      case 2           => "19/19/19/15S3/19/19/19/19/19/19/19/19/19/19/19/3S15/19/19/19"
      case 3           => "19/19/19/15S3/19/19/19/19/19/19/19/19/19/19/19/3S11S3/19/19/19"
      case 4           => "19/19/19/3S11S3/19/19/19/19/19/19/19/19/19/19/19/3S11S3/19/19/19"
      case 5           => "19/19/19/3S11S3/19/19/19/19/19/9S9/19/19/19/19/19/3S11S3/19/19/19"
      case 6           => "19/19/19/3S11S3/19/19/19/19/19/3S11S3/19/19/19/19/19/3S11S3/19/19/19"
      case 7           => "19/19/19/3S11S3/19/19/19/19/19/3S5S5S3/19/19/19/19/19/3S11S3/19/19/19"
      case 8           => "19/19/19/3S5S5S3/19/19/19/19/19/3S5S9/19/19/19/19/19/3S5S5S3/19/19/19"
      case 9           => "19/19/19/3S5S5S3/19/19/19/19/19/3S5S5S3/19/19/19/19/19/3S5S5S3/19/19/19"
      case 10          => "19/19/19/3S5S5S3/19/19/6S12/19/19/3S5S5S3/19/19/19/19/19/3S5S5S3/19/19/19"
      case 11          => "19/19/19/3S5S5S3/19/19/6S12/19/19/3S5S5S3/19/19/12S6/19/19/3S5S5S3/19/19/19"
      case 12          => "19/19/19/3S5S5S3/19/19/6S12/19/19/3S5S5S3/19/19/6S5S6/19/19/3S5S5S3/19/19/19"
      case 13          => "19/19/19/3S5S5S3/19/19/6S5S6/19/19/3S5S5S3/19/19/6S5S6/19/19/3S5S5S3/19/19/19"
      case 14          => "19/19/19/3S2S2S5S3/19/19/6S5S6/19/19/3S5S5S3/19/19/6S5S6/19/19/3S5S5S3/19/19/19"
      case 15          => "19/19/19/3S2S2S2S2S3/19/19/6S5S6/19/19/3S5S5S3/19/19/6S5S6/19/19/3S5S5S3/19/19/19"
      case 16          => "19/19/19/3S2S2S2S2S3/19/19/6S5S2S3/19/19/3S5S5S3/19/19/6S5S6/19/19/3S5S5S3/19/19/19"
      case 17          => "19/19/19/3S2S2S2S2S3/19/19/6S5S2S3/19/19/3S5S5S3/19/19/6S5S2S3/19/19/3S5S5S3/19/19/19"
      case 18          => "19/19/19/3S2S2S2S2S3/19/19/6S5S2S3/19/19/3S5S5S3/19/19/6S5S2S3/19/19/3S5S2S2S3/19/19/19"
      case 19          => "19/19/19/3S2S2S2S2S3/19/19/6S5S2S3/19/19/3S5S5S3/19/19/6S5S2S3/19/19/3S2S2S2S2S3/19/19/19"
      case 20          => "19/19/19/3S2S2S2S2S3/19/19/6S5S2S3/19/19/3S5S5S3/19/19/3S2S5S2S3/19/19/3S2S2S2S2S3/19/19/19"
      case 21          =>
        "19/19/19/3S2S2S2S2S3/19/19/3S2S5S2S3/19/19/3S5S5S3/19/19/3S2S5S2S3/19/19/3S2S2S2S2S3/19/19/19"
      case 22          =>
        "19/19/19/3S2S2S2S2S3/19/19/3S2S2S2S2S3/19/19/3S5S5S3/19/19/3S2S5S2S3/19/19/3S2S2S2S2S3/19/19/19"
      case 23          =>
        "19/19/19/3S2S2S2S2S3/19/19/3S2S2S2S2S3/19/19/3S5S2S2S3/19/19/3S2S5S2S3/19/19/3S2S2S2S2S3/19/19/19"
      case 24          =>
        "19/19/19/3S2S2S2S2S3/19/19/3S2S2S2S2S3/19/19/3S5S2S2S3/19/19/3S2S2S2S2S3/19/19/3S2S2S2S2S3/19/19/19"
      case 25          =>
        "19/19/19/3S2S2S2S2S3/19/19/3S2S2S2S2S3/19/19/3S2S2S2S2S3/19/19/3S2S2S2S2S3/19/19/3S2S2S2S2S3/19/19/19"
      case x if x > 25 =>
        "19/19/19/3S2S2S2S2S3/19/19/3S2S2S2S2S3/19/19/3S2S2S2S2S3/19/19/3S2S2S2S2S3/19/19/3S2S2S2S2S3/19/19/19"
      case _           => "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19"
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
