package strategygames.go

import org.specs2.mutable.Specification

import strategygames.Score
import strategygames.go.format.{ FEN, Forsyth, Uci }
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
      differences(named, situation.board.variant.boardAfter(situation, pos), enginePlacing(situation, pos))
    }

  private def placedPointOf(action: String): Option[Pos] = uciOf(action) match {
    case Uci.Drop(_, pos) => Some(pos)
    case _                => None
  }

  private def uciOf(action: String): Uci =
    Uci(action).getOrElse(sys.error(s"unreadable go uci: ${action}"))

  private def enginePlacing(situation: Situation, pos: Pos): Api.Position =
    Api.positionFromVariantStartingFenAndMoves(
      situation.board.variant,
      Forsyth.exportBoardFen(situation.board),
      List(s"${situation.board.variant.defaultRole.forsyth}@${pos.key}")
    )

  private def differences(named: String, ruled: Board, engined: Api.Position): List[String] =
    List(
      differingPieces(named, ruled, engined),
      difference(named, "ko", ruled.ko, engined.fen.ko),
      difference(named, "consecutivePasses", ruled.consecutivePasses, engined.fen.fenPassCount),
      difference(named, "score", ruled.history.score, engined.fenScore),
      difference(
        named,
        "captures",
        ruled.history.captures,
        Score(engined.fen.player1Captures, engined.fen.player2Captures)
      )
    ).flatten

  private def differingPieces(named: String, ruled: Board, engined: Api.Position): Option[String] = {
    val onlyRuled   = ruled.pieces.toSet -- engined.pieceMap.toSet
    val onlyEngined = engined.pieceMap.toSet -- ruled.pieces.toSet
    Option.when(onlyRuled.nonEmpty || onlyEngined.nonEmpty)(
      s"${named}: pieces only boardAfter has ${describedStones(onlyRuled)}, " +
        s"only the engine has ${describedStones(onlyEngined)}"
    )
  }

  private def describedStones(stones: Set[(Pos, Piece)]): String =
    stones.toList.map { case (pos, piece) => s"${pos.key}(${piece.player.name})" }.sorted.mkString(",")

  private def difference[A](named: String, field: String, ruled: A, engined: A): Option[String] =
    Option.when(ruled != engined)(s"${named}: boardAfter ${field} ${ruled}, engine ${engined}")
}
