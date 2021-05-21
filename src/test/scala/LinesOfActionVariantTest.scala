package chess

import variant.LinesOfAction
import chess.format.FEN

class LinesOfActionVariantTest extends ChessTest {

  "Lines Of Action" should {

    "Black win from position" in {
      val position = FEN("1qqqqqq1/Q6Q/Q6Q/Q6Q/Q6Q/Q6Q/Q6Q/8 b - - 0 1")
      val game     = fenToGame(position, LinesOfAction)
      game must beValid.like { case game => {
        game.situation.winner == Option(Black) must beTrue
      }}
    }

    "Both colours win" in {
      val position = FEN("1qqqqqq1/Q7/Q7/Q7/Q7/Q7/Q7/8 b - - 0 1")
      val game     = fenToGame(position, LinesOfAction)
      game must beValid.like { case game => {
        game.situation.board.variant.specialDraw(game.situation) must beTrue
      }}
    }

    "Game in progress" in {
      val position = FEN("1qqqqqq1/Q6Q/Q6Q/Q6Q/Q6Q/Q6Q/Q6Q/1qqqqqq1 b - - 0 1")
      val game     = fenToGame(position, LinesOfAction)
      game must beValid.like { case game => {
        game.situation.board.variant.specialEnd(game.situation) must beFalse
      }}
    }
  }
}
