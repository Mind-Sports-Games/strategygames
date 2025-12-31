package strategygames.fairysf

import strategygames.Player

class BreakthroughTroykaReplayTest extends FairySFTest {
  "BreakthroughTroyka" should {
    val initialFen = variant.BreakthroughTroyka.initialFen
    "not have winner from start position" in {
      val game = fenToGame(initialFen, variant.BreakthroughTroyka)
      game .toOption must beSome.like {
        case game => {
          game.situation.end must beFalse
          game.situation.status == None must beTrue
          game.situation.winner == None must beTrue
        }
      }
    }

    "P1 wins in example game" in {
      val breakthroughGame = Vector(
          // @formatter:off
          "a2b3", "h7h6",
          "b3a4", "h6h5",
          "a4b5", "h5h4",
          "b5a6", "h4h3",
          "a6b7", "h3g2",
          // @formatter:on
      ).map(Vector(_))

      val replay = Replay.gameWithUciWhileValid(breakthroughGame, initialFen, variant.BreakthroughTroyka)
      val game   = replay._2.last._1
      game.situation.board.variant.exportBoardFen(game.situation.board) === format.FEN(
        "pppppppp/pPppppp1/8/8/8/8/1PPPPPpP/PPPPPPPP w - - 0 6"
      )
      val game2  = game.apply(game.situation.moves(Pos.B7).filter(_.dest == Pos.A8)(0))
      game2.situation.board.variant.exportBoardFen(game2.situation.board) === format.FEN(
        "Pppppppp/p1ppppp1/8/8/8/8/1PPPPPpP/PPPPPPPP b - - 0 6"
      )
      game2.situation.end must beTrue
      game2.situation.winner === Some(Player.P1)
    }
  }

  "MiniBreakthroughTroyka" should {
    "not have winner from start position" in {
      val game = fenToGame(variant.BreakthroughTroyka.initialFen, variant.BreakthroughTroyka)
      game .toOption must beSome.like {
        case game => {
          game.situation.end must beFalse
          game.situation.status == None must beTrue
          game.situation.winner == None must beTrue
        }
      }
    }

    "P1 wins in example game" in {
      val breakthroughGame = Vector(
          // @formatter:off
          "a2b3", "e4e3",
          "b3a4", "e3d2"
          // @formatter:on
      ).map(Vector(_))

      val replay = Replay.gameWithUciWhileValid(
        breakthroughGame,
        variant.MiniBreakthroughTroyka.initialFen,
        variant.MiniBreakthroughTroyka
      )
      val game   = replay._2.last._1
      game.situation.board.variant.exportBoardFen(game.situation.board) === format.FEN(
        "ppppp/Pppp1/5/1PPpP/PPPPP w - - 0 3"
      )
      val game2  = game.apply(game.situation.moves(Pos.A4).filter(_.dest == Pos.B5)(0))
      game2.situation.board.variant.exportBoardFen(game2.situation.board) === format.FEN(
        "pPppp/1ppp1/5/1PPpP/PPPPP b - - 0 3"
      )
      game2.situation.end must beTrue
      game2.situation.winner === Some(Player.P1)
    }
  }
}
