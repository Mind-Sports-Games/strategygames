package strategygames.fairysf

import variant.Flipello
import strategygames.fairysf.format.FEN

class FlipelloVariantTest extends FairySFTest {

  // https://playstrategy.dev/4eCFkjwt
  val flipelloGameEndsEarly = List(
    "P@e3",
    "P@f5",
    "P@e6",
    "P@d7",
    "P@c6",
    "P@d3",
    "P@f4",
    "P@d6",
    "P@c5",
    "P@b5",
    "P@b7",
    "P@b6",
    "P@c7",
    "P@a8",
    "P@f6",
    "P@e7",
    "P@a7",
    "P@b8",
    "P@f7",
    "P@c8", // 10 moves each (20 ply)
    "P@c3",
    "P@a6",
    "P@a5",
    "P@a4",
    "c3c3", // first pass for P1 (1-1)
    "P@d8",
    "c3c3", // another pass for P1 (1-2)
    "P@b4",
    "P@a3",
    "P@a2",
    "c3c3", // second set of passing for P1 (2-1)
    "P@b3",
    "c3c3", // second set of passing for P1 (2-2)
    "P@c4",
    "c3c3", // second set of passing for P1 (2-3)
    "P@c2",
    "P@b2",
    "P@f3",
    "P@b1",
    "P@a1", // 20 moves each (40 ply)
    "P@g3",
    "P@c1",
    "d3d3", // third set of passing for P1 (3-1)
    "P@d2",
    "P@d1",
    "P@e1",
    "d3d3", // fourth set of passing for P1 (4-1)
    "P@e2",
    "e3e3", // fourth set of passing for P1 (4-2)
    "P@f2",
    "P@f1",
    "P@g1",
    "f3f3", // fifth set of passing for P1 (5-1)
    "P@h3",
    "P@g2",
    "P@h1",
    "P@h2",
    "P@g4",
    "P@h4",
    "P@h5", // 30 moves each (60 ply)
    "P@g5",
    "P@h6",
    "e5e5", // sixth set of passing for P1 (6-1)
    "P@g6",
    "P@g7",
    "a1a1", // P2 has moves but only has a valid move to pass for some reason!?
    "e5e5"  // P1 then has no moves and has to pass ending the game
  )

  "Flipello" should {

    "P2 win from position" in {
      val position =
        FEN("8/8/8/3pP3/3Pp3/8/8/8[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppp] w 0 1")
      val game     = fenToGame(position, Flipello)
      game must beValid.like {
        case game => {
          game.situation.winner == None must beTrue
        }
      }
    }

    "have legal moves even after passing" in {
      val position  = Api.positionFromVariant(variant.Flipello)
      val position2 = position.makeMoves(
        flipelloGameEndsEarly.take(65)
      ) // just before P2 move (which engine says to pass incorrectly)

      println("Possible moves in this position")
      position2.legalMoves.map(println(_))

      println("current fen")
      println(position2.fen) // checking number of drops remaining....

      position2.legalMoves.size must_== 5
      position2.gameEnd must_== false
    }

  }
}
