package strategygames.go

import org.specs2.mutable.Specification

import strategygames.{ Player, Score }
import strategygames.go.format.FEN
import strategygames.go.variant.{ Go13x13, Go19x19, Go9x9, Variant }

class GoLegalityTest extends Specification with GoRulesTestSupport {

  private val koSequence = List("g5", "f5", "f4", "e4", "f6", "d5", "h5", "e6", "e5")

  private val snapbackSetup = List("a3", "a2", "b3", "b2", "c2", "c1", "b1", "a1")

  "a point that already holds a stone" should {
    val afterBothColours = playing(Go9x9, List("e5", "d5"))
    "not be offered back to the player who filled it" in {
      dropKeysOf(afterBothColours.situation) must not(contain("e5"))
    }
    "not be offered to the opponent either" in {
      dropKeysOf(afterBothColours.situation) must not(contain("d5"))
    }
  }

  "a point whose only neighbours are enemy stones" should {
    val enclosedByWhite = playing(Go9x9, List("a2", "e5", "b1"))
    "not be offered, because the placement would capture nothing" in {
      dropKeysOf(enclosedByWhite.situation) must not(contain("a1"))
    }
    "be refused when asked for by name" in {
      enclosedByWhite.situation.drop(Role.defaultRole, pointAt("a1")).isInvalid === true
    }
  }

  "a point that would fill the last liberty of the chain it joins" should {
    val chainDownToOneLiberty = playing(Go9x9, List("a2", "a3", "i9", "b2", "i8", "b1"))
    "be illegal even though it touches a friendly stone" in {
      dropKeysOf(chainDownToOneLiberty.situation) must not(contain("a1"))
    }
    "be refused when asked for by name" in {
      chainDownToOneLiberty.situation.drop(Role.defaultRole, pointAt("a1")).isInvalid === true
    }
  }

  "a placement with no liberties of its own that captures" should {
    "be legal, because the capture is resolved before the liberties are counted" in {
      val betweenTwoDoomedChains = playing(Go9x9, List("a3", "a2", "b2", "b1", "c1", "e5"))
      dropKeysOf(betweenTwoDoomedChains.situation) must contain("a1")
    }
  }

  "a single stone capture" should {
    val afterKoCapture = playing(Go9x9, koSequence)
    "record the emptied point as the ko point" in {
      koPointOf(fenOf(afterKoCapture)) === "f5"
    }
    "record it, and withhold the recapture, on every board size the shape fits" in {
      forall(List[Variant](Go9x9, Go13x13, Go19x19)) { variant =>
        val onThisSize = playing(variant, koSequence)
        (koPointOf(fenOf(onThisSize)) === "f5") and
          (dropKeysOf(onThisSize.situation) must not(contain("f5"))) and
          (onThisSize.situation.history.captures === Score(1, 0))
      }
    }
    "withhold the immediate recapture" in {
      dropKeysOf(afterKoCapture.situation) must not(contain("f5"))
    }
    "refuse the immediate recapture when asked for by name" in {
      afterKoCapture.situation.drop(Role.defaultRole, pointAt("f5")).isInvalid === true
    }
    "credit the captured stone to the capturing player" in {
      afterKoCapture.situation.history.captures === Score(1, 0)
    }
  }

  "an exchange played away from the ko" should {
    val afterExchange = playing(Go9x9, koSequence ++ List("a1", "c9"))
    "release the ko point" in {
      koPointOf(fenOf(afterExchange)) === "-"
    }
    "hand the recapture back" in {
      dropKeysOf(afterExchange.situation) must contain("f5")
    }
    "move the ko point onto the square the recapture empties" in {
      val afterRecapture = playingOn(afterExchange, List("f5"))
      (koPointOf(fenOf(afterRecapture)) === "e5") and
        (afterRecapture.situation.history.captures === Score(1, 1)) and
        (dropKeysOf(afterRecapture.situation) must not(contain("e5")))
    }
  }

  "a pass" should {
    val liveKo      = playing(Go19x19, List("c1", "c4", "b2", "b3", "d2", "d3", "c3", "c2"))
    val afterKoPass = playingOn(liveKo, List("pass"))
    "clear the ko point it inherits" in {
      (koPointOf(fenOf(liveKo)) === "c3") and (koPointOf(fenOf(afterKoPass)) === "-")
    }
    "hand back the point the ko was withholding" in {
      (dropKeysOf(liveKo.situation) must not(contain("c3"))) and
        (dropKeysOf(afterKoPass.situation) must contain("c3"))
    }
    "leave every empty point of the board playable" in {
      (dropKeysOf(liveKo.situation).size === 353) and (dropKeysOf(afterKoPass.situation).size === 354)
    }
  }

  "(Issue#489) passing after a single stone capture" should {
    val beforePass = playing(Go9x9, List("e5", "e4", "d4", "pass", "e3", "pass", "f4"))
    val afterPass  = playingOn(beforePass, List("pass"))
    "leave 76 drops while the captured point is still unplayable" in {
      dropKeysOf(beforePass.situation).size === 76
    }
    "leave 77 drops once the pass has handed the move over" in {
      (dropKeysOf(afterPass.situation).size === 77) and
        (dropKeysOf(afterPass.situation) must contain("e4"))
    }
  }

  "a capture whose stone joins a bigger chain" should {
    val afterSnapbackCapture = playing(Go9x9, snapbackSetup)
    "record no ko point" in {
      koPointOf(fenOf(afterSnapbackCapture)) === "-"
    }
    "leave the recapture immediately available" in {
      dropKeysOf(afterSnapbackCapture.situation) must contain("b1")
    }
    "credit the single stone it took" in {
      afterSnapbackCapture.situation.history.captures === Score(0, 1)
    }
    "lose the whole chain to the snapback" in {
      val afterSnapback = playingOn(afterSnapbackCapture, List("b1"))
      (afterSnapback.situation.history.captures === Score(3, 1)) and
        (stoneAt(afterSnapback, "a1") === None) and
        (stoneAt(afterSnapback, "a2") === None) and
        (stoneAt(afterSnapback, "b2") === None) and
        (stoneAt(afterSnapback, "b1") === Some(Piece(P1, Stone))) and
        (stoneAt(afterSnapback, "c1") === Some(Piece(P2, Stone)))
    }
  }

  "a ko point read back from a fen" should {
    val loadedWithKoPoint = situationFrom(FEN(s"9/9/9/9/9/9/9/9/9${pocket} b c3 0 55 0 0 55 0 1"))
    "forbid the recapture it names, the position history behind it having been lost" in {
      dropKeysOf(loadedWithKoPoint) must not(contain("c3"))
    }
    "withhold that one point and no other" in {
      dropKeysOf(loadedWithKoPoint).size === 80
    }
    "survive being written back out" in {
      koPointOf(fenOf(loadedWithKoPoint)) === "c3"
    }
  }

  "(Issue#490) a single stone capture answered by a two stone recapture" should {
    val beforeRecapture = playing(
      Go9x9,
      List("e5", "e6", "d6", "e7", "d7", "e9", "e8", "f8", "f7", "d8", "f6", "h1", "e8", "e6", "g1", "e7")
    )
    "leave 69 drops with the recapture among them" in {
      (dropKeysOf(beforeRecapture.situation).size === 69) and
        (dropKeysOf(beforeRecapture.situation) must contain("e8"))
    }
    "take both stones when the recapture is played, and credit both" in {
      val afterRecapture = playingOn(beforeRecapture, List("e8"))
      (stoneAt(afterRecapture, "e8") === Some(Piece(P1, Stone))) and
        (stoneAt(afterRecapture, "e6") === None) and
        (stoneAt(afterRecapture, "e7") === None) and
        (afterRecapture.situation.history.captures ===
          beforeRecapture.situation.history.captures.add(P1, 2))
    }
    "record no ko point, since more than one stone came off" in {
      koPointOf(fenOf(playingOn(beforeRecapture, List("e8")))) === "-"
    }
  }
}
