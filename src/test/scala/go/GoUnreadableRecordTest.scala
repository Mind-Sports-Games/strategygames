package strategygames.go

import org.specs2.mutable.Specification

import strategygames.Player
import strategygames.go.format.Uci
import strategygames.go.variant.Go9x9

// NOTE: these records are illegal under every go ruleset rather than under a rule that changed, which
// is why they are kept while the superko and post-settlement cases beside them were dropped. Replay
// does not adjudicate a record, so the question each one asks is not "is this refused" but "does an
// unreadable record cost a game its tail or its whole load".
class GoUnreadableRecordTest extends Specification with GoRulesTestSupport {

  import GoUnreadableRecordTest._

  "a record naming a point that already holds a stone" should {

    "stop the replay and report it, rather than raising out of the load" in {
      val (_, plies, error) = replayed(onAnOccupiedPoint)
      (error must beSome) and (plies.size === 1)
    }

    "keep the plies it read before the unreadable one" in {
      val (_, plies, _) = replayed(onAnOccupiedPoint)
      plies.lastOption.map(_._1.board.pieces.size) === Some(1)
    }

    "name the action and the ply it stopped at" in {
      val (_, _, error) = replayed(onAnOccupiedPoint)
      error.map(_.contains("s@e5")) === Some(true)
    }
  }

  // NOTE: no go ruleset permits a placement that leaves its own chain without a liberty, so this is
  // not a record written under older rules — it is a record that cannot have been played. Replay
  // takes it as written, because `Variant.boardAfter` computes a board rather than judging one, and
  // the result is a position go cannot hold. Pinned because it is a known gap, not a decision.
  // TODO(playstrategy): refuse a suicide in `boardAfter` and this becomes a truncated replay too.
  "a record naming a suicide" should {

    "replay as written rather than stopping" in {
      val (_, _, error) = replayed(asSuicide)
      error === None
    }

    "leave a chain standing with no liberties, which go cannot hold" in {
      val (_, plies, _) = replayed(asSuicide)
      val board         = plies.last._1.board
      board.pieces.keys.count(point => !Chain.hasLiberty(board, Chain.at(board, point))) === 1
    }
  }
}

object GoUnreadableRecordTest {

  // p1 names e5 twice; the second placement has nowhere to go
  val onAnOccupiedPoint: List[String] = List("e5", "e5")

  // p1's a1 fills its own last liberty, taking nothing with it
  val asSuicide: List[String] = List("e5", "a2", "e6", "b1", "a1")

  private def asUci(action: String): String =
    if (action == "pass" || action.startsWith("ss:")) action else s"${Stone.forsyth}@${action}"

  private def turnPerAction(actions: List[String]) = actions.map(action => Vector(asUci(action))).toVector

  def replayed(actions: List[String]): (Game, List[(Game, Uci.WithSan)], Option[String]) =
    Replay.gameWithUciWhileValid(
      turnPerAction(actions),
      P1,
      Player.fromTurnCount(actions.size),
      Go9x9.initialFen,
      Go9x9
    )
}
