package strategygames.fairysf

import variant.BreakthroughTroyka
import variant.MiniBreakthroughTroyka

class BreakthroughTroykaApiTest extends FairySFTest {

  "BreakthroughTroyka initial position" should {
    val position = Api.positionFromVariant(BreakthroughTroyka)

    "have 32 pieces" in {
      position.pieceMap.size must_== 32
    }
    "should have 22 legal moves" in {
      position.legalMoves.size must_== 22L
    }
    "not have winner" in {
      val fen  = BreakthroughTroyka.initialFen
      val game = fenToGame(fen, BreakthroughTroyka)
      game must beValid.like {
        case game => {
          game.situation.winner == None must beTrue
        }
      }
    }
  }

  "BreakthroughTroyka quickest win for white" should {
    val position  = Api.positionFromVariant(BreakthroughTroyka)
    val position2 = position.makeMoves(
      List(
        // @formatter:off
        "a2b3", "h7h6",
        "b3a4", "h6h5",
        "a4b5", "h5h4",
        "b5a6", "h4h3",
        "a6b7", "h3g2",
        "b7a8"
        // @formatter:on
      )
    )

    "produce no legal move" in {
      position2.legalMoves.size must_== 0L
    }

    "result in a win" in {
      position2.gameEnd must_== true
    }
  }

  "BreakthroughTroyka quickest win for black" should {
    val position  = Api.positionFromVariant(BreakthroughTroyka)
    val position2 = position.makeMoves(
      List(
        // @formatter:off
        "a2b3", "h7h6",
        "b3a4", "h6h5",
        "a4b5", "h5h4",
        "b5a6", "h4h3",
        "a6b7", "h3g2",
        "a1a2", "g2h1"
        // @formatter:on
      )
    )

    "produce no legal move" in {
      position2.legalMoves.size must_== 0L
    }

    "result in a win" in {
      position2.gameEnd must_== true
    }
  }

  "MiniBreakthroughTroyka initial position" should {
    val position = Api.positionFromVariant(MiniBreakthroughTroyka)

    "have 20 pieces" in {
      position.pieceMap.size must_== 20
    }
    "have 13 legal moves" in {
      position.legalMoves.size must_== 13L
    }
    "not have winner" in {
      val fen  = MiniBreakthroughTroyka.initialFen
      val game = fenToGame(fen, MiniBreakthroughTroyka)
      game must beValid.like {
        case game => {
          game.situation.winner == None must beTrue
        }
      }
    }
  }

  "MiniBreakthroughTroyka quickest win" should {
    val position = Api.positionFromVariant(MiniBreakthroughTroyka)
    // position.legalMoves.map(e => { println(e); e; })

    val position2 = position.makeMoves(
      List(
        // @formatter:off
        "a2b3", "e4d3",
        "b3a4", "d3e2",
        "a4b5"//, "e2d1"
        // @formatter:on
      )
    )

    "produce no legal moves" in {
      position2.legalMoves.size must_== 0L
    }
    "result in a win" in {
      val game = fenToGame(position2.fen, MiniBreakthroughTroyka)
      game must beValid.like {
        case game => {
          // TODO: you'll need to get some form of this working for breakthrough
          game.situation.checkMate must_== true
          game.situation.winner must beSome.like(
            winner => winner must_== P1
          )
        }
      }
    }
  }

}
