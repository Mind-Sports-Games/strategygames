package strategygames.fairysf

import variant.Amazons
import strategygames.fairysf.format.FEN

class AmazonsVariantTest extends FairySFTest {

  // https://playstrategy.dev/4eCFkjwt
  val amazonsGame = List(
    "d1d6,d6g9",
    "d10f10,f10e10",
    "a4f9,f9e9",
    "a7a10,a10b10",
    "d6a9,a9b9",
    "j7j10,j10h10",
    "j4j9,j9h9",
    "j10i10,i10i9",
    "j9j10,j10j9" // then p2 has no moves
  )

  "Amazons" should {

    "not have winner from start position" in {
      val position =
        FEN(
          "3q2q3/10/10/q8q/10/10/Q8Q/10/10/3Q2Q3[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppp] w - - 0 1"
        )
      val game     = fenToGame(position, Amazons)
      game must beValid.like {
        case game => {
          game.situation.winner == None must beTrue
        }
      }
    }

    "2176 move from start pos" in {
      val position = Api.positionFromVariant(variant.Amazons)
      position.legalMoves.size must_== 2176
    }

    "have 32 pawn drops from starting d1d6 move" in {
      val position = Api.positionFromVariant(variant.Amazons)
      position.legalMoves.filter(x => x.contains("d1d6")).size must_== 32
    }

    "have openning move d1d6,d6g9" in {
      val position = Api.positionFromVariant(variant.Amazons)
      position.legalMoves.contains("d1d6,d6g9")
    }

    "P1 win in example game" in {
      val position  = Api.positionFromVariant(variant.Amazons)
      val position2 = position.makeMoves(amazonsGame)
      println("current fen")
      println(position2.fen)
      position2.legalMoves.size must_== 0
      position2.gameEnd must_== true
    }

    "Should detect game end from fen" in {
      val position = FEN(
        "qp2pqqpqQ/QP2PQPPpP/10/10/10/10/10/10/10/6Q3[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppp] b - - 9 5"
      )
      val game     = fenToGame(position, Amazons)
      game must beValid.like {
        case game => {
          game.situation.playable(false) must beFalse
        }
      }
    }

    "Have 25 moves from position" in {
      val fen      = FEN(
        "q*2*qq*q1/Q*2*Q***1/10/10/10/10/10/10/10/6Q2Q[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppp] b - - 9 5"
      )
      val position = Api.positionFromVariantNameAndFEN(variant.Amazons.key, fen.toString())
      println("current fen")
      println(position.fen)
      // position.legalMoves.map(println(_))
      position.legalMoves.size must_== 26
    }

  }
}
