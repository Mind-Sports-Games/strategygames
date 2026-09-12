package strategygames.go

import org.specs2.mutable.Specification

import scala.util.Try

import strategygames.Player

class GoStoredRepetitionTest extends Specification with GoRulesTestSupport {

  private val endingOnARepeat = List("gKIjsXZc", "wH9kQR7E")

  private val anyFurtherAction = "s@a1"

  private lazy val all: List[StoredGoGame] = endingOnARepeat.map(StoredGoGames.named)

  private def replayed(game: StoredGoGame, actions: List[String]): Game =
    Replay
      .gameFromUciStrings(
        actions.map(Vector(_)).toVector,
        Player.fromTurnCount(game.startPly + actions.size),
        Some(game.initialFen),
        game.variant
      )
      .valueOr(error => sys.error(error))

  private def beforeTheRepeat(game: StoredGoGame): Game = replayed(game, game.actions.init)

  private def theRepeat(game: StoredGoGame): Pos = pointAt(game.actions.last.drop(2))

  "the stored go games ending on a repeated position" should {

    "end on a placement" in {
      forall(all) { game => game.actions.last must startWith("s@") }
    }

    "not offer that placement to the player who made it" in {
      forall(all) { game =>
        dropKeysOf(beforeTheRepeat(game).situation) must not(contain(theRepeat(game).key))
      }
    }

    "refuse that placement when it is asked for by name" in {
      forall(all) { game =>
        beforeTheRepeat(game).situation.drop(Role.defaultRole, theRepeat(game)).isInvalid === true
      }
    }

    "offer that placement again from the same position read back from a fen" in {
      forall(all) { game =>
        val reached = situationFrom(fenOf(beforeTheRepeat(game)))
        dropKeysOf(reached) must contain(theRepeat(game).key)
      }
    }

    "refuse that placement when the record continues past it" in {
      forall(all) { game =>
        Try(replayed(game, game.actions :+ anyFurtherAction)) must beAFailedTry
      }
    }

    "end on that placement" in {
      forall(all) { game => replayed(game, game.actions).situation.end === true }
    }

    "offer no drop once it has ended" in {
      forall(all) { game => dropKeysOf(replayed(game, game.actions).situation) === Nil }
    }

    "come back from replay under the rules play uses" in {
      forall(all) { game => replayed(game, game.actions).board.ruleset === Ruleset.AsCurrentlyPlayed }
    }
  }
}
