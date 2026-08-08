package strategygames.go

import org.specs2.mutable.Specification

import strategygames.{ Player, Score }
import strategygames.format.pgn.{ Tag, Tags }
import strategygames.go.variant.Go9x9

class GoSettlementCaptureTest extends Specification with GoRulesTestSupport {

  import GoSettlementCaptureTest._

  "a settlement that lifts one stone" should {

    "count no captures at all when it is played" in {
      playing(Go9x9, settlingScript).situation.history.captures === Score(0, 0)
    }

    "count one more capture than it lifts when it is replayed, on every loader" in {
      (capturesReplayedFromUci === Score(liftedPlusOne, 0)) and
        (capturesReadFromActionStrs === Score(liftedPlusOne, 0))
    }

    "leave the played and the replayed game disagreeing by that count, deliberately" in {
      (playing(Go9x9, settlingScript).situation.history.captures === Score(0, 0)) and
        (capturesReplayedFromUci === Score(liftedPlusOne, 0))
    }
  }

  "a settlement that lifts nothing" should {

    "still count one capture on every replay loader" in {
      (settledEmptyCaptures(replayedFromUci(emptySettlingScript)) === Score(1, 0)) and
        (settledEmptyCaptures(readFromActionStrs(emptySettlingScript)) === Score(1, 0))
    }
  }

  private def capturesReplayedFromUci    = replayedFromUci(settlingScript).situation.history.captures
  private def capturesReadFromActionStrs = readFromActionStrs(settlingScript).situation.history.captures

  private def settledEmptyCaptures(game: Game) = game.situation.history.captures
}

object GoSettlementCaptureTest {

  val settlingScript              = List("a1", "e5", "pass", "pass", "ss:a1")
  private val emptySettlingScript = List("a1", "e5", "pass", "pass", "ss:")

  private def asUci(action: String): String =
    if (action == "pass" || action.startsWith("ss:")) action else s"${Stone.forsyth}@${action}"

  private def turnPerAction(actions: List[String]) = actions.map(action => Vector(asUci(action))).toVector

  private val liftedPlusOne = 2

  private val goTags = Tags(List(Tag(_.Variant, Go9x9.name)))

  private def activePlayerAfter(actions: List[String]) = Player.fromTurnCount(actions.size)

  def replayedFromUci(actions: List[String]): Game =
    Replay
      .gameFromUciStrings(turnPerAction(actions), activePlayerAfter(actions), None, Go9x9)
      .valueOr(error => sys.error(s"uci replay of ${actions.mkString(" ")}: ${error}"))

  def readFromActionStrs(actions: List[String]): Game =
    format.pgn.Reader
      .replayResultFromActionStrs(turnPerAction(actions), identity, goTags)
      .andThen(_.valid)
      .map(_.state)
      .valueOr(error => sys.error(s"pgn reader replay of ${actions.mkString(" ")}: ${error}"))
}
