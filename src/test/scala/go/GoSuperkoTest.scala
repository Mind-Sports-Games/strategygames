package strategygames.go

import org.specs2.mutable.Specification

import strategygames.Score
import strategygames.go.variant.Go9x9

class GoSuperkoTest extends Specification with GoRulesTestSupport {

  private val koShapeWithWhiteToPlay = List("b2", "c2", "a3", "d3", "b4", "c4", "c3")

  private val upToTheSilentRepeat =
    List("c2", "c4", "b1", "c1", "d2", "d3", "a3", "a2", "b3", "d4", "a1", "b2", "b1", "a1", "b1")

  private val silentRepeat = "a2"

  "the returning capture of a send two return one cycle" should {
    val afterCycle = playing(Go9x9, List("d1", "a2", "c2", "b2", "a1", "pass", "b1", "c1", "a1", "pass"))
    "never be offered" in {
      dropKeysOf(afterCycle.situation) must not(contain("b1"))
    }
    "be refused when asked for by name" in {
      afterCycle.situation.drop(Role.defaultRole, pointAt("b1")).isInvalid === true
    }
    "have cost the cycling player two stones on the way round" in {
      afterCycle.situation.history.captures === Score(0, 2)
    }
  }

  "a capture that recreates the board of three plies earlier, with the other player to move" should {
    val beforeReturn  = playing(
      Go9x9,
      List("i9", "i8", "h9", "h8", "g8", "i7", "f9", "g9", "h9", "i6", "i9", "g9")
    )
    val threePliesAgo = playing(
      Go9x9,
      List("i9", "i8", "h9", "h8", "g8", "i7", "f9", "g9", "h9", "i6")
    )
    "stand on a board with no ko point" in {
      koPointOf(fenOf(beforeReturn)) === "-"
    }
    "be offered" in {
      dropKeysOf(beforeReturn.situation) must contain("h9")
    }
    "reach exactly that board, with the other player to move" in {
      val returned = playingOn(beforeReturn, List("h9"))
      (returned.board.pieces === threePliesAgo.board.pieces) and
        (returned.situation.player === !threePliesAgo.situation.player)
    }
    "leave the game ongoing and free of repetition when a pass is played instead" in {
      val afterPass = playingOn(beforeReturn, List("pass"))
      (afterPass.situation.end === false) and (afterPass.situation.isRepetition === false)
    }
  }

  "a placement that takes no stone and still recreates an earlier position" should {
    val beforeTheRepeat = playing(Go9x9, upToTheSilentRepeat)
    val sixPliesEarlier = playing(Go9x9, upToTheSilentRepeat.take(10))
    "take no stone" in {
      Chain.capturedBy(
        beforeTheRepeat.board,
        beforeTheRepeat.situation.player,
        pointAt(silentRepeat)
      ) must beEmpty
    }
    "recreate the position of six plies earlier, with the same player to move" in {
      val returned = Go9x9.boardAfter(beforeTheRepeat.situation, pointAt(silentRepeat))
      (returned.pieces === sixPliesEarlier.board.pieces) and
        (!beforeTheRepeat.situation.player === sixPliesEarlier.situation.player)
    }
    "not be offered by drop generation" in {
      dropKeysOf(beforeTheRepeat.situation) must not(contain(silentRepeat))
    }
    "be refused when asked for by name" in {
      beforeTheRepeat.situation.drop(Role.defaultRole, pointAt(silentRepeat)).isInvalid === true
    }
  }

  "a recapture whose simple ko point has lapsed under two passes" should {
    val afterKoCaptureAndBothPasses =
      playing(Go9x9, koShapeWithWhiteToPlay ++ List("b3", "pass", "pass"))
    "leave no ko point on the board at all" in {
      koPointOf(fenOf(afterKoCaptureAndBothPasses)) === "-"
    }
    "still be refused, because the position it recreates is in the history" in {
      dropKeysOf(afterKoCaptureAndBothPasses.situation) must not(contain("c3"))
    }
    "still be refused when asked for by name" in {
      afterKoCaptureAndBothPasses.situation.drop(Role.defaultRole, pointAt("c3")).isInvalid === true
    }
  }

  "the position history that superko is enforced from" should {
    val afterKoCaptureAndBothPasses =
      playing(Go9x9, koShapeWithWhiteToPlay ++ List("b3", "pass", "pass"))
    val reloaded                    = situationFrom(fenOf(afterKoCaptureAndBothPasses))
    "not survive a fen round trip, which leaves the same board permitting the recapture" in {
      (dropKeysOf(afterKoCaptureAndBothPasses.situation) must not(contain("c3"))) and
        (dropKeysOf(reloaded) must contain("c3"))
    }
    "round trip everything the fen does carry" in {
      fenOf(reloaded) === fenOf(afterKoCaptureAndBothPasses)
    }
  }
}
