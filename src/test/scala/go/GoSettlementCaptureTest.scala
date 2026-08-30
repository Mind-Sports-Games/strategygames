package strategygames.go

import org.specs2.mutable.Specification

import strategygames.{ Player, Score }
import strategygames.format.pgn.{ Tag, Tags }
import strategygames.go.format.Uci
import strategygames.go.variant.Go9x9

class GoSettlementCaptureTest extends Specification with GoRulesTestSupport {

  import GoSettlementCaptureTest._

  "a settlement that lifts one stone" should {

    "count no captures at all when it is played" in {
      playing(Go9x9, settlingScript).situation.history.captures === Score(0, 0)
    }

    "count one more capture than it lifts on the loaders that fold action strings" in {
      (capturesOf(gameFromUciStrings(settlingScript)) === Score(liftedPlusOne, 0)) and
        (capturesOf(readerFromActionStrs(settlingScript)) === Score(liftedPlusOne, 0))
    }

    "count nothing on the loaders that replay a uci list through the played path" in {
      (capturesOf(replayFromUciList(settlingScript)) === Score(0, 0)) and
        (capturesOfLast(situationsFromUciList(settlingScript)) === Score(0, 0))
    }

    "leave the played game and the two groups of loaders disagreeing, deliberately" in {
      (playing(Go9x9, settlingScript).situation.history.captures === Score(0, 0)) and
        (capturesOf(gameFromUciStrings(settlingScript)) === Score(liftedPlusOne, 0)) and
        (capturesOf(replayFromUciList(settlingScript)) === Score(0, 0))
    }
  }

  "a settlement that lifts nothing" should {

    "still count one capture on the loaders that fold action strings" in {
      (capturesOf(gameFromUciStrings(emptySettlingScript)) === Score(1, 0)) and
        (capturesOf(readerFromActionStrs(emptySettlingScript)) === Score(1, 0))
    }

    "count nothing on the loaders that replay a uci list" in {
      (capturesOf(replayFromUciList(emptySettlingScript)) === Score(0, 0)) and
        (capturesOfLast(situationsFromUciList(emptySettlingScript)) === Score(0, 0))
    }
  }
}

object GoSettlementCaptureTest {

  val settlingScript              = List("a1", "e5", "pass", "pass", "ss:a1")
  private val emptySettlingScript = List("a1", "e5", "pass", "pass", "ss:")

  private def asUci(action: String): String =
    if (action == "pass" || action.startsWith("ss:")) action else s"${Stone.forsyth}@${action}"

  private def turnPerAction(actions: List[String]) = actions.map(action => Vector(asUci(action))).toVector

  private def uciList(actions: List[String]) = actions.flatMap(action => Uci(asUci(action)))

  private val liftedPlusOne = 2

  private val goTags = Tags(List(Tag(_.Variant, Go9x9.name)))

  private def activePlayerAfter(actions: List[String]) = Player.fromTurnCount(actions.size)

  def capturesOf(game: Game): Score = game.situation.history.captures

  def capturesOfLast(situations: List[Situation]): Score = situations.last.history.captures

  def gameFromUciStrings(actions: List[String]): Game =
    Replay
      .gameFromUciStrings(turnPerAction(actions), activePlayerAfter(actions), None, Go9x9)
      .valueOr(error => sys.error(s"uci replay of ${actions.mkString(" ")}: ${error}"))

  def readerFromActionStrs(actions: List[String]): Game =
    format.pgn.Reader
      .replayResultFromActionStrs(turnPerAction(actions), identity, goTags)
      .andThen(_.valid)
      .map(_.state)
      .valueOr(error => sys.error(s"pgn reader replay of ${actions.mkString(" ")}: ${error}"))

  def replayFromUciList(actions: List[String]): Game =
    Replay
      .apply(uciList(actions), None, Go9x9)
      .map(_.state)
      .valueOr(error => sys.error(s"uci list replay of ${actions.mkString(" ")}: ${error}"))

  def situationsFromUciList(actions: List[String]): List[Situation] =
    Replay
      .situationsFromUci(uciList(actions), None, Go9x9)
      .valueOr(error => sys.error(s"uci list situations of ${actions.mkString(" ")}: ${error}"))
}
