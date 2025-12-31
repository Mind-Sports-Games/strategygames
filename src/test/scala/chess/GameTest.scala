package strategygames.chess

import Pos._

class GameTest extends ChessTest {

  "prevent castle by capturing a rook" should {
    val game = Game(
      """
 b
R   K""",
      P2
    )
    "can castle queenside" in {
      (game.board.history canCastle P1 on QueenSide) === true
    }
    "can still castle queenside" in {
      game.playMoves(B2 -> A3).toOption must beSome.like { case g =>
        (g.board.history canCastle P1 on QueenSide) === true
      }
    }
    "can not castle queenside anymore" in {
      game.playMoves(B2 -> A1).toOption must beSome.like { case g =>
        (g.board.history canCastle P1 on QueenSide) === false
      }
    }
  }

  "update half move clock" should {
    "start at 0" in {
      Game(variant.Standard).halfMoveClock === 0
    }
    "increment" in {
      Game(variant.Standard)(G1, F3) .toOption must beSome.like { case (game, _) =>
        game.halfMoveClock === 1
      }
    }
    "not increment" in {
      Game(variant.Standard)(E2, E4) .toOption must beSome.like { case (game, _) =>
        game.halfMoveClock === 0
      }
    }
  }
}
