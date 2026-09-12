package strategygames.go

import org.specs2.mutable.Specification

import scala.util.Try

import strategygames.Player
import strategygames.go.variant.Go9x9

class GoReplayToleranceTest extends Specification with GoRulesTestSupport {

  import GoSituationalSuperkoTest._

  private def turnPerAction(actions: List[String]) = actions.map(Vector(_)).toVector

  private def strictly(actions: List[String]): Try[Game] =
    Try(
      Replay
        .gameFromUciStrings(
          turnPerAction(actions),
          Player.fromTurnCount(actions.size),
          None,
          Go9x9
        )
        .valueOr(sys.error)
    )

  private def tolerantly(actions: List[String]): ToleratedGame =
    Replay
      .toleratedGameFromUciStrings(
        turnPerAction(actions),
        Player.fromTurnCount(actions.size),
        None,
        Go9x9
      )
      .valueOr(sys.error)

  private def theOnlyRefusal(actions: List[String]): String =
    tolerantly(actions).tolerances match {
      case List(tolerance) => tolerance.refusal
      case many            => sys.error(s"expected one tolerance, got ${many}")
    }

  private val aGameEndedByFourPasses =
    dropsOf(List("e5", "a1")) ++ List("pass", "pass", "pass", "pass")

  private val aGameEndedByASettlement =
    dropsOf(List("e5", "a1")) ++ List("pass", "pass", "ss:i9")

  "a record the rules accept" should {

    "replay to the same game strictly and tolerantly" in {
      val actions = dropsOf(upToTheReturningCapture)
      fenOf(tolerantly(actions).game).value === fenOf(strictly(actions).get).value
    }

    "report no tolerance" in {
      tolerantly(dropsOf(upToTheReturningCapture)).tolerances must beEmpty
    }
  }

  "a record whose actions a named tolerance already covers" should {

    "report no tolerance for a run of passes past the fourth" in {
      tolerantly(aGameEndedByFourPasses).tolerances must beEmpty
    }

    "report no tolerance for a repeat that closes it" in {
      tolerantly(dropsOf(tripleKoReturningToTheSamePlayer)).tolerances must beEmpty
    }
  }

  "a record no named tolerance covers" should {

    "replay a placement on an occupied point, naming the point" in {
      theOnlyRefusal(dropsOf(onAnOccupiedPoint)) must
        endWith("cannot perform the drop: Stone on e5")
    }

    "replay a placement that is suicide, naming the point" in {
      theOnlyRefusal(dropsOf(asSuicide)) must endWith("cannot perform the drop: Stone on a1")
    }

    "replay a recapture at the simple ko point, naming the point" in {
      theOnlyRefusal(dropsOf(atTheSimpleKoPoint)) must
        endWith("cannot perform the drop: Stone on c3")
    }

    "replay an action after a repeat that ended the game, naming the point" in {
      theOnlyRefusal(dropsOf(tripleKoReturningToTheSamePlayer :+ anyFurtherPoint)) must
        endWith(s"cannot perform the drop: Stone on ${anyFurtherPoint}")
    }

    "replay a pass offered to a finished game, naming the pass" in {
      theOnlyRefusal(aGameEndedByASettlement :+ "pass") must
        startWith("Variant(Go 9x9) variant cannot pass a finished")
    }

    "name the ply the override happened at" in {
      tolerantly(dropsOf(onAnOccupiedPoint)).tolerances.map(_.ply) === List(1)
    }

    "replay every action of it" in {
      tolerantly(dropsOf(onAnOccupiedPoint)).game.plies === onAnOccupiedPoint.size
    }

    "still be refused strictly" in {
      strictly(dropsOf(onAnOccupiedPoint)) must beAFailedTry
    }
  }

  "live play" should {

    "refuse what a tolerant replay overrides" in {
      val afterTheFirst = playing(Go9x9, onAnOccupiedPoint.take(1))
      afterTheFirst.situation.drop(Role.defaultRole, pointAt(onAnOccupiedPoint.last)).isInvalid === true
    }
  }
}
