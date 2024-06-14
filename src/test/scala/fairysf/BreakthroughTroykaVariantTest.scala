package strategygames.fairysf

import strategygames.Player

class BreakthroughTroykaVariantTest extends FairySFTest {
  "BreakthroughTroyka" should {
    val initialFen = variant.BreakthroughTroyka.initialFen
    "not have winner from start position" in {
      val game = fenToGame(initialFen, variant.BreakthroughTroyka)
      println(game.map(g => g.situation.board.pieces))
      game must beValid.like {
        case game => {
          game.situation.end must beFalse
          game.situation.status == None must beTrue
          game.situation.winner == None must beTrue
        }
      }
    }

    "P1 win in example game" in {
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
      game.situation.board.variant.exportBoardFen(game.situation.board) must_== format.FEN(
        "pppppppp/pPppppp1/8/8/8/8/1PPPPPpP/PPPPPPPP w - - 0 6"
      )
      val game2  = game.apply(game.situation.moves(Pos.B7).filter(_.dest == Pos.A8)(0))
      game2.situation.board.variant.exportBoardFen(game2.situation.board) must_== format.FEN(
        "Pppppppp/p1ppppp1/8/8/8/8/1PPPPPpP/PPPPPPPP b - - 0 6"
      )
      game2.situation.pp("test situation")
      game2.situation.end must beTrue
      game2.situation.winner must_== Some(Player.P1)
    }
  }
}
