package strategygames.chess

import variant.LinesOfAction
import strategygames.chess.format.FEN

class LinesOfActionVariantTest extends ChessTest {

  "Lines Of Action" should {

    "P2 win from position" in {
      val position = FEN("1llllll1/L6L/L6L/L6L/L6L/L6L/L6L/8 b - - 0 1")
      val game     = fenToGame(position, LinesOfAction)
      game must beValid.like {
        case game => {
          game.situation.winner == Option(P2) must beTrue
        }
      }
    }

    "Both colours win" in {
      val position = FEN("1llllll1/L7/L7/L7/L7/L7/L7/8 b - - 0 1")
      val game     = fenToGame(position, LinesOfAction)
      game must beValid.like {
        case game => {
          game.situation.board.variant.specialDraw(game.situation) must beTrue
        }
      }
    }

    "Game in progress" in {
      val position = FEN("1llllll1/L6L/L6L/L6L/L6L/L6L/L6L/1llllll1 b - - 0 1")
      val game     = fenToGame(position, LinesOfAction)
      game must beValid.like {
        case game => {
          game.situation.board.variant.specialEnd(game.situation) must beFalse
        }
      }
    }
  }
}
