package strategygames.go

import org.specs2.mutable.Specification

import strategygames.Score
import strategygames.go.variant.{ Go13x13, Go19x19, Go9x9, Variant }

class GoCaptureTest extends Specification with GoRulesTestSupport {

  private val captureBothWays =
    List("g5", "f5", "f4", "e4", "f6", "d5", "h5", "e6", "e5") ++ List("a1", "c9", "f5")

  "a stone in the corner" should {
    "be taken on every board size" in {
      forall(
        List[(Variant, String, String, String)](
          (Go9x9, "a2", "a1", "b1"),
          (Go13x13, "l1", "m1", "m2"),
          (Go19x19, "r19", "s19", "s18")
        )
      ) { case (variant, approach, cornered, closing) =>
        val afterCapture = playing(variant, List(approach, cornered, closing))
        (stoneAt(afterCapture, cornered) === None) and
          (afterCapture.situation.history.captures === Score(1, 0)) and
          (koPointOf(fenOf(afterCapture)) === "-") and
          (afterCapture.situation.player === P2)
      }
    }
  }

  "one stone filling the last liberty of two chains at once" should {
    val afterDoubleCapture = playing(Go9x9, List("a3", "a2", "b2", "b1", "c1", "e5", "a1"))
    "take both chains" in {
      (stoneAt(afterDoubleCapture, "a2") === None) and
        (stoneAt(afterDoubleCapture, "b1") === None) and
        (stoneAt(afterDoubleCapture, "a1") === Some(Piece(P1, Stone)))
    }
    "credit both stones to the mover" in {
      afterDoubleCapture.situation.history.captures === Score(2, 0)
    }
    "record no ko point, since more than one stone came off" in {
      koPointOf(fenOf(afterDoubleCapture)) === "-"
    }
  }

  "prisoners" should {
    val afterCapturesBothWays = playing(Go9x9, captureBothWays)
    "accumulate against the player whose stone was taken" in {
      afterCapturesBothWays.situation.history.captures === Score(1, 1)
    }
    "survive the passes that open dead stone selection" in {
      playingOn(afterCapturesBothWays, List("pass", "pass")).situation.history.captures === Score(1, 1)
    }
    "survive the settlement that ends the game" in {
      playingOn(afterCapturesBothWays, List("pass", "pass", "ss:")).situation.history.captures ===
        Score(1, 1)
    }
    "play no part in the score, which counts the emptied point as territory instead" in {
      val afterPrisonerTaken = playing(Go9x9, List("a1", "b1", "pass", "a2"))
      val scored             = fenOf(afterPrisonerTaken)
      (afterPrisonerTaken.situation.history.captures === Score(0, 1)) and
        (scored.player1Score === 0) and
        (scored.player2Score === 865)
    }
  }

  "a capture" should {
    val afterCapture =
      playing(Go9x9, List("e5", "d5", "e6", "f5", "a9", "e4", "d4", "d6", "f4", "f6", "a8", "e7"))
    "empty the intersections it took" in {
      (stoneAt(afterCapture, "e5") === None) and
        (stoneAt(afterCapture, "e6") === None) and
        (afterCapture.situation.history.captures === Score(0, 2))
    }
    "free those intersections for the next drop" in {
      (dropKeysOf(afterCapture.situation) must contain("e5")) and
        (dropKeysOf(afterCapture.situation) must contain("e6")) and
        (dropKeysOf(afterCapture.situation).size === 71)
    }
  }
}
