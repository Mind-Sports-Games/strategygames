package strategygames.fairysf

import variant.Amazons
import strategygames.fairysf.format.FEN

class AmazonsVariantTest extends FairySFTest {

  // https://playstrategy.dev/4eCFkjwt
  val amazonsGame = List(
    "d1d6pg9",
    "d10f10pe10",
    "a4f9pe9",
    "a7a10pb10",
    "d6a9pb9",
    "j7j10ph10",
    "j4j9ph9",
    "j10i10pi9",
    "j9j10pj9" // then p2 has no moves
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

    "play d1d6pc8 and opponent have moves" in {
      val position  = Api.positionFromVariant(variant.Amazons)
      val position2 = position.makeMoves(List("d1d6pc8"))
      //   println("Possible moves in this position")
      //   position2.legalMoves.map(println(_))

      //   println("current fen")
      //   println(position2.fen)

      //   println(position2.legalMoves.size)

      position2.legalMoves.size > 0

    }

    "have 32 pawn drops from starting d1d6 move" in {
      val position = Api.positionFromVariant(variant.Amazons)

      //   println("Possible moves in this position")
      //   position.legalMoves.filter(x => x.contains("d1d6")).map(println(_))

      //   println("current fen")
      //   println(position.fen)

      position.legalMoves.filter(x => x.contains("d1d6")).size must_== 32
    }

    "have openning move d1d6pg9" in {
      val position = Api.positionFromVariant(variant.Amazons)
      // val position2 = position.makeMoves(amazonsGame.take(1))

      //   println("Possible moves in this position")
      //   position2.legalMoves.map(println(_))

      //   println("current fen")
      //   println(position2.fen)

      position.legalMoves.contains("d1d6pg9")
    }

    // "P1 win in example game" in {
    //   val position  = Api.positionFromVariant(variant.Amazons)
    //   val position2 = position.makeMoves(amazonsGame)

    //   //   println("Possible moves in this position")
    //   //   position2.legalMoves.map(println(_))

    //   //   println("current fen")
    //   //   println(position2.fen)

    //   position2.legalMoves.size must_== 0
    //   position2.gameEnd must_== true
    // }

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
        "qp2pqqpq1/QP2PQPPp1/10/10/10/10/10/10/10/6Q2Q[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppp] b - - 9 5"
      )
      val position = Api.positionFromVariantNameAndFEN(variant.Amazons.key, fen.toString())
      position.legalMoves.map(println(_))
      position.legalMoves.size must_== 25

    }

  }
}
