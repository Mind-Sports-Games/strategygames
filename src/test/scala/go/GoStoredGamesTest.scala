package strategygames.go

import org.specs2.mutable.Specification

import strategygames.{ Player, Status }

class GoStoredGamesTest extends Specification with GoRulesTestSupport {

  import GoStoredGamesTest._


  private def replayed(game: StoredGoGame): Either[String, Game] =
    Replay
      .gameFromUciStrings(
        game.actions.map(Vector(_)).toVector,
        Player.fromTurnCount(game.plies),
        Some(game.initialFen),
        game.variant
      )
      .leftMap(refusal => s"${game.id}: ${refusal}")
      .toEither

  private def resultOf(played: Game): String = {
    val score = played.situation.board.areaScore
    List(
      s"end=${played.situation.end}",
      s"status=${played.situation.status.fold("none")(_.toString)}",
      s"winner=${played.situation.winner.fold("none")(_.fold("p1", "p2"))}",
      s"score=${score.p1},${score.p2}",
      s"plies=${played.plies}"
    ).mkString(" ")
  }

  private lazy val replays: List[(StoredGoGame, Either[String, Game])] =
    StoredGoGames.all.map(game => (game, replayed(game)))

  private lazy val replayedById: Map[String, Either[String, Game]] =
    replays.map { case (game, replay) => (game.id, replay) }.toMap

  private def replayOf(record: LilaRecord): Either[String, Game] =
    replayedById.getOrElse(record.id, sys.error(s"${record.id} is not in the stored go corpus"))

  private def storedAs(status: Status): List[LilaRecord] =
    lilaRecords.filter(_.status == status.id)

  "the stored go corpus" should {

    "hold every game dumped from the dev server and from production" in {
      StoredGoGames.all.size === 63
    }

    "cover all three board sizes" in {
      StoredGoGames.all.map(_.variant.key).distinct.sorted === List("go13x13", "go19x19", "go9x9")
    }

    "hold a drop set hash for the starting position and for every ply after it" in {
      forall(StoredGoGames.all) { game =>
        game.snapshottedDropSetHashes.size === game.actions.size + 1
      }
    }
  }

  "every stored go game" should {

    "replay without a rule refusing a ply it was played with" in {
      forall(replays) { case (_, replay) => replay must beRight }
    }

    "replay to the ply it was stored at" in {
      forall(replays) { case (game, replay) =>
        replay must beRight { (played: Game) => played.plies === game.plies }
      }
    }

    "reach the position it reached when it was snapshotted" in {
      forall(replays) { case (game, replay) =>
        replay must beRight { (played: Game) =>
          fenOf(played).value === game.snapshottedFinalFen.value
        }
      }
    }

    "reach the result it reached when it was snapshotted" in {
      forall(replays) { case (game, replay) =>
        replay must beRight { (played: Game) => resultOf(played) === game.snapshottedResult }
      }
    }

    "come back from replay under the rules play uses" in {
      forall(replays) { case (_, replay) =>
        replay must beRight { (played: Game) => played.board.ruleset === Ruleset.AsCurrentlyPlayed }
      }
    }
  }

  "every game the rewritten replay refused before this corpus was built" should {

    "be one lila holds a status and a turn count for" in {
      lilaRecords.map(_.id).sorted === lilaRecords.map(_.id).distinct.sorted and
        (lilaRecords.size === 13)
    }

    "replay to the turn count lila stored" in {
      forall(lilaRecords) { record =>
        replayOf(record) must beRight { (played: Game) => played.plies === record.turns }
      }
    }

    "report the variant end lila stored" in {
      forall(storedAs(Status.VariantEnd)) { record =>
        replayOf(record) must beRight { (played: Game) =>
          played.situation.status === Some(Status.VariantEnd)
        }
      }
    }

    "report no rules end where lila stored a resignation" in {
      forall(storedAs(Status.Resign)) { record =>
        replayOf(record) must beRight { (played: Game) => played.situation.status === None }
      }
    }
  }
}

object GoStoredGamesTest {

  case class LilaRecord(id: String, status: Int, turns: Int)

  val lilaRecords: List[LilaRecord] = List(
    LilaRecord("5YVWHmbP", 60, 356),
    LilaRecord("7OOMRoTr", 60, 107),
    LilaRecord("7b9ecrac", 60, 127),
    LilaRecord("8DFCmqGs", 60, 159),
    LilaRecord("DKwuwpYC", 60, 99),
    LilaRecord("K6C2JgRm", 60, 154),
    LilaRecord("XIDrmYiV", 60, 88),
    LilaRecord("dkDHkTtG", 60, 583),
    LilaRecord("e9Vdd8IS", 60, 88),
    LilaRecord("gKIjsXZc", 60, 478),
    LilaRecord("pNGZoBUY", 31, 140),
    LilaRecord("q0G93H1w", 60, 64),
    LilaRecord("wH9kQR7E", 60, 101)
  )
}
