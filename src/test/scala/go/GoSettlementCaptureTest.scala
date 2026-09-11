package strategygames.go

import org.specs2.mutable.Specification

import strategygames.{ Player, Score }
import strategygames.format.pgn.{ Tag, Tags }
import strategygames.go.format.Uci
import strategygames.go.variant.Go9x9

class GoSettlementCaptureTest extends Specification with GoRulesTestSupport {

  import GoSettlementCaptureTest._

  // every route a settled go game is reached by: played live, and the four loaders
  private def everyPathThrough(actions: List[String]): List[(String, Score)] =
    List(
      "played live"         -> playing(Go9x9, actions).situation.history.captures,
      "gameFromUciStrings"  -> capturesOf(gameFromUciStrings(actions)),
      "the pgn reader"      -> capturesOf(readerFromActionStrs(actions)),
      "a uci list replay"   -> capturesOf(replayFromUciList(actions)),
      "uci list situations" -> capturesOfLast(situationsFromUciList(actions))
    )

  "a settlement that lifts a stone" should {
    "record no captures, on every path a game reaches it by" in {
      forall(everyPathThrough(settlingScript)) { case (path, captures) =>
        captures aka s"captures via ${path}" must be_==(Score(0, 0))
      }
    }
  }

  "a settlement that lifts nothing" should {
    "record no captures either" in {
      forall(everyPathThrough(emptySettlingScript)) { case (path, captures) =>
        captures aka s"captures via ${path}" must be_==(Score(0, 0))
      }
    }
  }

  "the stones a drop took before the passes" should {
    "survive the settlement, and read the same on every path" in {
      forall(everyPathThrough(captureThenSettleScript)) { case (path, captures) =>
        captures aka s"captures via ${path}" must be_==(Score(1, 0))
      }
    }
  }
}

object GoSettlementCaptureTest {

  private val settlingScript      = List("a1", "e5", "pass", "pass", "ss:a1")
  private val emptySettlingScript = List("a1", "e5", "pass", "pass", "ss:")

  // p1 takes the cornered stone on b1, then both players pass the game out and settle
  private val captureThenSettleScript = List("a2", "a1", "b1", "pass", "pass", "ss:")

  private def asUci(action: String): String =
    if (action == "pass" || action.startsWith("ss:")) action else s"${Stone.forsyth}@${action}"

  private def turnPerAction(actions: List[String]) = actions.map(action => Vector(asUci(action))).toVector

  private def uciList(actions: List[String]) = actions.flatMap(action => Uci(asUci(action)))

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
