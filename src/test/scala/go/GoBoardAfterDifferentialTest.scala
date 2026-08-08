package strategygames.go

import org.specs2.mutable.Specification

import strategygames.go.format.{ FEN, Uci }
import strategygames.go.oracle.{ GoOracle, GoOracleGame }
import strategygames.go.variant.Variant

class GoBoardAfterDifferentialTest extends Specification {

  private val mismatchReportLimit = 25

  private val placementsInTheFixture = 15919

  private lazy val comparedPlacements: List[List[String]] = GoOracle.load().flatMap(comparisonsOf)

  private lazy val recordedPlacements: Int =
    GoOracle.load().flatMap(_.actionStrs).flatMap(placedPointOf).size

  "applying a placement" should {
    "agree with the engine at every drop of every game the golden oracle holds" in {
      comparedPlacements.flatten.take(mismatchReportLimit) must beEmpty
    }
    "leave no drop of the golden oracle uncompared" in {
      comparedPlacements.size === recordedPlacements
    }
    "find in the golden oracle every drop the committed fixture holds" in {
      recordedPlacements === placementsInTheFixture
    }
  }

  private def comparisonsOf(recording: GoOracleGame): List[List[String]] =
    recording.actionStrs.zipWithIndex
      .foldLeft((startingGame(recording), List.empty[List[String]])) {
        case ((game, compared), (action, ply)) =>
          (
            played(game, action),
            compared ++ comparisonAt(s"${recording.name} ply ${ply}", game.situation, action)
          )
      }
      ._2

  private def startingGame(recording: GoOracleGame): Game =
    Game(Some(variantOf(recording)), recording.initialFen.map(FEN(_)))

  private def variantOf(recording: GoOracleGame): Variant =
    Variant(recording.variantKey)
      .getOrElse(sys.error(s"unknown go variant: ${recording.variantKey}"))

  private def played(game: Game, action: String): Game =
    game
      .apply(uciOf(action))
      .map { case (afterwards, _) => afterwards }
      .getOrElse(sys.error(s"illegal go action: ${action}"))

  private def comparisonAt(named: String, situation: Situation, action: String): List[List[String]] =
    placedPointOf(action).toList.map { pos =>
      differences(
        named,
        situation.board.variant.boardAfter(situation, pos),
        situation.board.afterDrop(situation.player, pos)
      )
    }

  private def placedPointOf(action: String): Option[Pos] = uciOf(action) match {
    case Uci.Drop(_, pos) => Some(pos)
    case _                => None
  }

  private def uciOf(action: String): Uci =
    Uci(action).getOrElse(sys.error(s"unreadable go uci: ${action}"))

  private def differences(named: String, ruled: Board, engined: Board): List[String] =
    List(
      differingPieces(named, ruled, engined),
      difference(named, "ko", ruled.ko, engined.ko),
      difference(named, "consecutivePasses", ruled.consecutivePasses, engined.consecutivePasses),
      difference(named, "score", ruled.history.score, engined.history.score),
      difference(named, "captures", ruled.history.captures, engined.history.captures),
      difference(named, "halfMoveClock", ruled.history.halfMoveClock, engined.history.halfMoveClock),
      difference(named, "positionCount", ruled.history.positionCount, engined.history.positionCount),
      difference(named, "currentPosition", ruled.history.currentPosition, engined.history.currentPosition)
    ).flatten

  private def differingPieces(named: String, ruled: Board, engined: Board): Option[String] = {
    val onlyRuled   = ruled.pieces.toSet -- engined.pieces.toSet
    val onlyEngined = engined.pieces.toSet -- ruled.pieces.toSet
    Option.when(onlyRuled.nonEmpty || onlyEngined.nonEmpty)(
      s"${named}: pieces only boardAfter has ${describedStones(onlyRuled)}, " +
        s"only afterDrop has ${describedStones(onlyEngined)}"
    )
  }

  private def describedStones(stones: Set[(Pos, Piece)]): String =
    stones.toList.map { case (pos, piece) => s"${pos.key}(${piece.player.name})" }.sorted.mkString(",")

  private def difference[A](named: String, field: String, ruled: A, engined: A): Option[String] =
    Option.when(ruled != engined)(s"${named}: boardAfter ${field} ${ruled}, afterDrop ${engined}")
}
