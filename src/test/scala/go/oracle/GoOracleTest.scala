package strategygames.go.oracle

import org.specs2.mutable.Specification

import strategygames.go.format.Uci

class GoOracleTest extends Specification {

  import GoOracleTest._

  "the committed go oracle fixture" should {

    "survive a round trip through its own codec" in {
      GoOracle.parse(GoOracle.render(games)) === games
    }

    "name every game it holds, once each" in {
      (games.map(_.name).distinct.size === games.size) and
        (games.map(_.name).filter(_.isEmpty) === Nil) and
        (games.map(_.name).take(2) === List("upstream-0", "upstream-1")) and
        (games.map(_.name).filter(_.startsWith("walk-go9x9-")).take(1) === List("walk-go9x9-00")) and
        (games.map(_.name).filter(_.startsWith("handicap-")).size === 9)
    }

    "cover every go variant, the upstream suite and the deterministic walks" in {
      (games.map(_.variantKey).distinct.sorted === List("go13x13", "go19x19", "go9x9")) and
        (games.count(_.initialFen.isDefined) === 9) and
        (games.count(_.recordsFen) === 25) and
        (games.count(_.recordsDropKeys) === 3) and
        (games.size === 85)
    }

    "hold the stone placements of record, alongside its passes and its settlements" in {
      (games.flatMap(_.actionStrs).count(isPlacement) === placementsOfRecord) and
        (games.flatMap(_.actionStrs).size === actionsOfRecord)
    }

    "spell out the fen of every game that pins a named rule, and of no walk" in {
      (games.filter(_.recordsFen).flatMap(_.plies).forall(_.fen.nonEmpty) === true) and
        (games.filterNot(_.recordsFen).flatMap(_.plies).forall(_.fen.isEmpty) === true) and
        (games.filterNot(_.recordsFen).map(_.name).forall(_.startsWith("walk-")) === true)
    }

    "spell out legal drop keys for the ko scripts and for nothing else" in {
      (games.filter(_.recordsDropKeys).map(_.name) ===
        List("superko-corpus-9x9", "ko-point-19x19", "triple-ko")) and
        (games.filterNot(_.recordsDropKeys).flatMap(_.plies).forall(_.legalDropKeys.isEmpty) === true)
    }

    "keep the digests of a spelled out ply true to what they digest" in {
      games.filter(_.recordsFen).flatMap(_.plies).forall { ply =>
        ply.fenDigest == GoOracle.digestOf(ply.fen)
      } === true
    }

    "keep the drop count and digest of a spelled out ply true to its keys" in {
      games.filter(_.recordsDropKeys).flatMap(_.plies).forall { ply =>
        ply.legalDropCount == ply.legalDropKeys.size &&
        ply.legalDropDigest == GoOracle.digestOf(ply.legalDropKeys)
      } === true
    }

    "replay to every recorded field of every recorded ply" in {
      mismatches.take(mismatchReportLimit) must beEmpty
    }
  }
}

object GoOracleTest {

  private val mismatchReportLimit = 25

  val placementsOfRecord = 15919

  val actionsOfRecord = 16122

  lazy val games: List[GoOracleGame] = GoOracle.load()

  def isPlacement(actionStr: String): Boolean =
    Uci(actionStr).exists(_.isInstanceOf[Uci.Drop])

  lazy val mismatches: List[String] = games.flatMap(mismatchesOf)

  private def mismatchesOf(game: GoOracleGame): List[String] = {
    val replayed = GoOracle.replayedPlies(game)
    if (replayed.size != game.plies.size)
      List(s"${game.name}: recorded ${game.plies.size} plies, replayed ${replayed.size}")
    else
      game.plies.zip(replayed).zipWithIndex.flatMap { case ((recorded, played), ply) =>
        mismatchedFields(s"${game.name} ply ${ply}", recorded, played)
      }
  }

  private def mismatchedFields(named: String, recorded: GoOraclePly, played: GoOraclePly): List[String] =
    List(
      mismatched(named, "fen", recorded.fen, played.fen),
      mismatched(named, "fenDigest", recorded.fenDigest, played.fenDigest),
      mismatched(named, "legalDropCount", recorded.legalDropCount, played.legalDropCount),
      mismatched(named, "legalDropDigest", recorded.legalDropDigest, played.legalDropDigest),
      mismatched(named, "legalDropKeys", recorded.legalDropKeys, played.legalDropKeys),
      mismatched(named, "scoreP1", recorded.scoreP1, played.scoreP1),
      mismatched(named, "scoreP2", recorded.scoreP2, played.scoreP2),
      mismatched(named, "capturesP1", recorded.capturesP1, played.capturesP1),
      mismatched(named, "capturesP2", recorded.capturesP2, played.capturesP2),
      mismatched(named, "end", recorded.end, played.end),
      mismatched(named, "winner", recorded.winner, played.winner)
    ).flatten

  private def mismatched[A](named: String, field: String, recorded: A, played: A): Option[String] =
    if (recorded == played) None
    else Some(s"${named} ${field}: recorded ${describe(recorded)}, replayed ${describe(played)}")

  private def describe(value: Any): String = value match {
    case keys: List[_] => s"${keys.size} keys ${keys.take(8).mkString(",")}"
    case other         => other.toString
  }
}
