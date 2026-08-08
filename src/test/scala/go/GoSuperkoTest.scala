package strategygames.go

import org.specs2.mutable.Specification

import scala.util.Try

import strategygames.{ Player, Score }
import strategygames.go.format.Uci
import strategygames.go.variant.Go9x9

class GoSuperkoTest extends Specification with GoRulesTestSupport {

  private val tripleKo = List(
    "b8",
    "b7",
    "c9",
    "c6",
    "d8",
    "d7",
    "f8",
    "f7",
    "g9",
    "g6",
    "h8",
    "h7",
    "b2",
    "b3",
    "c1",
    "c4",
    "d2",
    "d3",
    "f2",
    "f3",
    "g1",
    "g4",
    "h2",
    "h3",
    "c7",
    "c2",
    "g3",
    "g8",
    "g7",
    "c8",
    "c3",
    "g2",
    "c7",
    "c2",
    "g3"
  )

  private def replaying(keys: List[String]) =
    Replay(keys.flatMap(key => Uci(s"s@${key}")), Some(Go9x9.initialFen), Go9x9)

  private def replayingFromUciStrings(keys: List[String]) =
    Replay
      .gameFromUciStrings(
        keys.map(key => Vector(s"s@${key}")).toVector,
        Player.fromTurnCount(keys.size),
        Some(Go9x9.initialFen),
        Go9x9
      )
      .valueOr(error => sys.error(error))

  private def refusalOf(replaying: => Any): String =
    Try(replaying).failed.map(_.getMessage).getOrElse("nothing was refused")

  private val koShapeWithWhiteToPlay = List("b2", "c2", "a3", "d3", "b4", "c4", "c3")

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

  "a triple ko that walks the board back to an earlier position" should {
    "replay while the repeating capture is still one ply away" in {
      replaying(tripleKo.init).isValid === true
    }
    "refuse the ply that repeats" in {
      replaying(tripleKo).isInvalid === true
    }
    "keep the position history the refusal is drawn from when rebuilt from its uci strings" in {
      replayingFromUciStrings(tripleKo.init).situation.history.positionCount === tripleKo.init.size + 1
    }
    "be refused at the repeating ply when rebuilt from its uci strings too" in {
      refusalOf(replayingFromUciStrings(tripleKo)) must
        startWith(s"Illegal action s@${tripleKo.last} at ply ${tripleKo.size - 1}")
    }
  }

  "a capture that recreates an earlier board across a pass parity flip" should {
    val beforeReturn = playing(
      Go9x9,
      List("i9", "i8", "h9", "h8", "g8", "i7", "f9", "g9", "h9", "i6", "i9", "g9")
    )
    "not be offered, though no ko point stands" in {
      (koPointOf(fenOf(beforeReturn)) === "-") and
        (dropKeysOf(beforeReturn.situation) must not(contain("h9")))
    }
    "leave the game ongoing and free of repetition when a pass is played instead" in {
      val afterPass = playingOn(beforeReturn, List("pass"))
      (afterPass.situation.end === false) and (afterPass.situation.isRepetition === false)
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
