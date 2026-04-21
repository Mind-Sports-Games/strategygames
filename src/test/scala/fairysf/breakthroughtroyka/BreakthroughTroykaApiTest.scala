package strategygames.fairysf

import variant.BreakthroughTroyka
import strategygames.fairysf.variant.MiniBreakthroughTroyka

class BreakthroughTroykaApiTest extends FairySFTest {

  "BreakthroughTroyka initial position" should {
    val position = Api.positionFromVariant(BreakthroughTroyka)

    "have 32 pieces" in {
      position.pieceMap.size === 32
    }
    "should have 22 legal moves" in {
      position.legalMoves.size === 22
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
      position2.legalMoves.size === 0
    }

    "should produce game end" in {
      position2.gameEnd === true
      position2.immediateGameEnd === true
    }

    "should be stalemate for api" in {
      position2.gameResult === GameResult.Stalemate() // because this is not Chess
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
      position2.legalMoves.size === 0
    }

    "should produce game end" in {
      position2.gameEnd === true
      position2.immediateGameEnd === true
    }

    "should be stalemate for api" in {
      position2.gameResult === GameResult.Stalemate() // because this is not Chess (resultFromInt)
    }
  }

  "MiniBreakthroughTroyka quickest win for white" should {
    val position  = Api.positionFromVariant(MiniBreakthroughTroyka)
    val position2 = position.makeMoves(
      List(
        // @formatter:off
        "a2b3", "e4e3",
        "b3a4", "e3d2",
        "a4b5"
        // @formatter:on
      )
    )

    "produce no legal move" in {
      position2.legalMoves.size === 0
    }

    "should produce game end" in {
      position2.gameEnd === true
      position2.immediateGameEnd === true
    }

    "should be stalemate for api" in {
      position2.gameResult === GameResult.Stalemate() // because this is not Chess (resultFromInt)
    }
  }

  "MiniBreakthroughTroyka quickest win for black" should {
    val position  = Api.positionFromVariant(MiniBreakthroughTroyka)
    val position2 = position.makeMoves(
      List(
        // @formatter:off
        "a2b3", "e4e3",
        "b3a4", "e3d2",
        "a1a2", "d2c1"
        // @formatter:on
      )
    )

    "produce no legal move" in {
      position2.legalMoves.size === 0
    }

    "should produce game end" in {
      position2.gameEnd === true
      position2.immediateGameEnd === true
    }

    "should be stalemate for api" in {
      position2.gameResult === GameResult.Stalemate() // because this is not Chess (resultFromInt)
    }
  }
}
