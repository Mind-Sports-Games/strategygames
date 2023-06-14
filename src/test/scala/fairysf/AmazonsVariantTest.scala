package strategygames.fairysf

import variant.Amazons
import strategygames.fairysf.format.FEN

import strategygames.{ Player, Status }

class AmazonsVariantTest extends FairySFTest {

  val initialFen = FEN(
    "3q2q3/10/10/q8q/10/10/Q8Q/10/10/3Q2Q3[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppp] w - - 0 1"
  )

  val amazonsGame = Vector(
    "d1d6",
    "P@g9",
    "d10f10",
    "P@e10",
    "a4f9",
    "P@e9",
    "a7a10",
    "P@b10",
    "d6a9",
    "P@b9",
    "j7j10",
    "P@h10",
    "j4j9",
    "P@h9",
    "j10i10",
    "P@i9" // then after j9j10,P@j9 p2 has no moves
  ).map(Vector(_))

  "Amazons" should {

    "not have winner from start position" in {
      val game = fenToGame(initialFen, Amazons)
      game must beValid.like {
        case game => {
          game.situation.end must beFalse
          game.situation.status == None must beTrue
          game.situation.winner == None must beTrue
        }
      }
    }

    "80 move from start pos" in {
      // val game = fenToGame(initialFen, Amazons)
      val game = Game(Amazons)
      game.situation.moves.size must_== 4
      game.situation.moves.toList(0)._2.size must_== 20
      game.situation.moves.toList.map(_._2.size).sum must_== 80
      // strategygames.fairysf.Game.apply(strategygames.fairysf.variant.Amazons).situation.moves.toList.map(_._2.size).sum
    }

    "P1 win in example game" in {
      val replay  = Replay.gamePlyWhileValid(amazonsGame, initialFen, variant.Amazons)
      val game    = replay._2.last._1
      val actions = game.actions
      game.situation.moves.keys.size must_== 4
      game.situation.moves.keys.filter(_ == Pos.J9).size must_== 1
      game.situation.moves(Pos.J9).filter(_.dest == Pos.J10).size must_== 1
      val game2   = game.apply(game.situation.moves(Pos.J9).filter(_.dest == Pos.J10)(0))
      game2.situation.dropsAsDrops.size must_== 9
      game2.situation.dropsAsDrops.filter(_.pos.key == "j9").size must_== 1
      val gameEnd = game2.applyDrop(game2.situation.dropsAsDrops.filter(_.pos.key == "j9")(0))
      gameEnd.situation.board.variant.exportBoardFen(game.situation.board) must_== FEN(
        "qp2pqqpq1/QP2PQPPpQ/10/10/10/10/10/10/10/6Q3[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppp] w - - 8 5"
      )
      gameEnd.situation.end must beTrue
      gameEnd.situation.status must_== Some(Status.VariantEnd)
      gameEnd.situation.winner must_== Some(Player.P1)
    }

  }
}
