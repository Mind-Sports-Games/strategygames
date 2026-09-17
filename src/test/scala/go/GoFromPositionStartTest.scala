package strategygames.go

import org.specs2.mutable.Specification

import strategygames.go.format.{ FEN, Forsyth }
import strategygames.go.variant.Go9x9

// NOTE: the position lila builds a challenge from, transcribed from `ChallengeJoiner.createGame`,
// which takes `startedAtTurn` from the `SituationPlus` this returns. The fen is one lila stores for
// a go game set up from a position; it names fewer fields than the fens this library writes.
class GoFromPositionStartTest extends Specification with GoRulesTestSupport {

  private val fromPosition = FEN(s"9/9/2S3S2/9/9/9/9/9/9${pocket} w - 81 4 4 1")

  "a go game started from a position lila stores" should {

    "be readable at all" in {
      Forsyth.<<<@(Go9x9, fromPosition).isDefined === true
    }

    "start at the turn the fen names, which is what lila records as startedAtTurn" in {
      Forsyth.<<<@(Go9x9, fromPosition).map(_.turnCount) === Some(1)
    }

    "start at the same ply" in {
      Forsyth.<<<@(Go9x9, fromPosition).map(_.plies) === Some(1)
    }

    "offer every empty point as a drop" in {
      Forsyth.<<<@(Go9x9, fromPosition).map(_.situation.dropsAsDrops.size) === Some(79)
    }
  }

  // `ChallengeJoiner.createGame` reads the position through the wrapper, not through `go.format`.
  "the wrapper lila reads the position through" should {

    def throughWrapper =
      strategygames.format.Forsyth.<<<@(
        strategygames.GameLogic.Go(),
        strategygames.variant.Variant.Go(Go9x9),
        strategygames.format.FEN.Go(fromPosition)
      )

    "hand lila the turn it records as startedAtTurn" in {
      throughWrapper.map(_.turnCount) === Some(1)
    }

    "hand lila the ply it records as startedAtPly" in {
      throughWrapper.map(_.plies) === Some(1)
    }

    "hand lila every empty point as a drop" in {
      throughWrapper.map(_.situation.dropsAsDrops.size) === Some(79)
    }
  }
}
