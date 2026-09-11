package strategygames.go

import org.specs2.mutable.Specification

import scala.util.Try

import strategygames.Player
import strategygames.format.pgn.{ Tag, Tags }
import strategygames.go.format.Uci
import strategygames.go.variant.Go9x9

class GoPostSettlementTest extends Specification with GoRulesTestSupport {

  private val settledGame = List("s@a1", "s@e5", "pass", "pass", "ss:a1")

  private val actionsOfferedAfterIt = List("pass", "ss:e5", "s@b2")

  private val goTags = Tags(List(Tag(_.Variant, Go9x9.name)))

  private def turnPerAction(actions: List[String]) = actions.map(Vector(_)).toVector

  private def refusalPerAction(refuses: List[String] => Boolean): List[(String, Boolean)] =
    actionsOfferedAfterIt.map(action => (action, refuses(settledGame :+ action)))

  private val everyOneRefused = actionsOfferedAfterIt.map((_, true))

  private def throwsFrom(load: List[String] => Any): List[String] => Boolean =
    actions => Try(load(actions)).isFailure

  private def gameFromUciStrings(actions: List[String]) =
    Replay
      .gameFromUciStrings(turnPerAction(actions), Player.fromTurnCount(actions.size), None, Go9x9)
      .valueOr(sys.error)

  private def replayFromActionStrs(actions: List[String]) =
    Replay
      .apply(turnPerAction(actions), P1, Player.fromTurnCount(actions.size), None, Go9x9)
      .andThen(_.valid)
      .valueOr(sys.error)

  private def readerFromActionStrs(actions: List[String]) =
    format.pgn.Reader
      .replayResultFromActionStrs(turnPerAction(actions), identity, goTags)
      .andThen(_.valid)
      .valueOr(sys.error)

  private def replayFromUciList(actions: List[String]) =
    Replay(actions.flatMap(Uci(_)), None, Go9x9)

  private def situationsFromUciList(actions: List[String]) =
    Replay.situationsFromUci(actions.flatMap(Uci(_)), None, Go9x9)

  "a pass, a second settlement or a drop offered after a settlement" should {

    "be refused by Replay.gameFromUciStrings" in {
      refusalPerAction(throwsFrom(gameFromUciStrings)) === everyOneRefused
    }

    "be refused by Replay.apply on action strings" in {
      refusalPerAction(throwsFrom(replayFromActionStrs)) === everyOneRefused
    }

    "be refused by pgn.Reader.replayResultFromActionStrs" in {
      refusalPerAction(throwsFrom(readerFromActionStrs)) === everyOneRefused
    }

    "be refused by Replay.apply on a uci list" in {
      refusalPerAction(actions => replayFromUciList(actions).isInvalid) === everyOneRefused
    }

    "be refused by Replay.situationsFromUci" in {
      refusalPerAction(actions => situationsFromUciList(actions).isInvalid) === everyOneRefused
    }
  }

  "the settlement itself" should {

    "still be accepted by every loader" in {
      (Try(gameFromUciStrings(settledGame)).isSuccess === true) and
        (Try(replayFromActionStrs(settledGame)).isSuccess === true) and
        (Try(readerFromActionStrs(settledGame)).isSuccess === true) and
        (replayFromUciList(settledGame).isValid === true) and
        (situationsFromUciList(settledGame).isValid === true)
    }
  }
}
