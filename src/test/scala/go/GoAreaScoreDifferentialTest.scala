package strategygames.go

import org.specs2.mutable.Specification

import strategygames.{ Player, Score }
import strategygames.go.format.{ FEN, Forsyth }
import strategygames.go.oracle.{ GoOracle, GoOracleGame, GoOraclePly }
import strategygames.go.variant.{ Go13x13, Go19x19, Go9x9, Variant }

class GoAreaScoreDifferentialTest extends Specification with GoRulesTestSupport {

  import GoAreaScoreDifferentialTest._

  private val blackWallWithOneWhiteStone =
    FEN(s"4S4/4S4/4S4/4S4/4S1s2/4S4/4S4/4S4/4S4${pocket} b - 450 65 0 0 55 2 1")

  private val blackWallAfterTheDeadStoneCameOff =
    FEN(s"${List.fill(9)("4S4").mkString("/")}${pocket} b - 810 55 0 1 55 3 1")

  private val singlePointEye = FEN(s"8s/9/9/9/9/9/9/S8/1S7${pocket} b - 30 65 0 0 55 0 1")

  private val wallsSharingEveryDamePointUnderNoKomi =
    FEN(s"${List.fill(9)("3S1s3").mkString("/")}${pocket} b - 360 360 0 0 0 0 1")

  private val equalAreaUnderSixPointKomi =
    FEN(s"SSSSSS13/${List.fill(17)("19").mkString("/")}/S3s14${pocket} b - 70 70 0 0 60 0 2")

  private val decidedButTwoPointsLeft = FEN(
    "2SSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/S1S1S1S1S1S1S1S1S1S/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/S1S1S1S1S1S1S1S1S1S[SSSSSSSSSSssssssssss] w - 3610 75 0 0 75 0 239"
  )

  private val decidedButTwentyEightPointsLeft = FEN(
    "s1s1s1s1s1s1s1s1s1s/sssssssssssssssssss/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/S1S1S1S1S1S1S1S1S1S/SSSSSSSSSSSSSSSSSSS/S1S1S1S1S1S1S1S1S1S/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/19[SSSSSSSSSSssssssssss] w - 0 75 0 0 75 0 239"
  )

  private val decidedButThirtyEightPointsLeft = FEN(
    "s1s1s1s1s1s1s1s1s1s/sssssssssssssssssss/1SSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/S1S1S1S1S1S1S1S1S1S/SSSSSSSSSSSSSSSSSSS/S1S1S1S1S1S1S1S1S1S/SSSSSSSSSSSSSSSSSSS/SSSSSSSSSSSSSSSSSSS/19[SSSSSSSSSSssssssssss] b - 0 75 0 0 75 0 239"
  )

  private val settledUnderSixAndAHalfPointKomi = FEN(
    s"${(List("s1" * 9 + "s", "s" * 19, "1" + "S" * 18) ++
        List.fill(10)("S" * 19) ++
        List("S1" * 9 + "S", "S" * 19, "S1" * 9 + "S", "S" * 19, "S" * 19, "19")).mkString("/")}${pocket} b - 3220 445 0 0 65 0 1"
  )

  private val komiTenthsByVariant = List[(Variant, Int)]((Go9x9, 55), (Go13x13, 75), (Go19x19, 75))

  "area scoring" should {
    "agree with the golden oracle at every ply of every game it holds" in {
      mismatches.take(mismatchReportLimit) must beEmpty
    }
  }

  "an empty board" should {
    "hand neither player any area, leaving p2 holding nothing but komi" in {
      forall(komiTenthsByVariant) { case (variant, komiTenths) =>
        areaScoreOf(Board.init(variant)) === Score(0, komiTenths)
      }
    }
  }

  "a lone black stone" should {
    "take every point of an otherwise empty board" in {
      areaScoreOf(playing(Go9x9, List("e5")).board) === Score(810, 55)
    }
  }

  "a region that both colours touch" should {
    "belong to nobody, leaving each player only the stone they played" in {
      areaScoreOf(playing(Go9x9, List("a1", "b1")).board) === Score(10, 65)
    }
  }

  "a region enclosed against black that only white touches" should {
    "belong to nobody, leaving white nothing but its own stone and komi" in {
      areaScoreOf(situationFrom(blackWallWithOneWhiteStone).board) === Score(450, 65)
    }
  }

  "a single point eye" should {
    "belong to the player enclosing it" in {
      areaScoreOf(situationFrom(singlePointEye).board) === Score(30, 65)
    }
  }

  "a stone that was captured" should {
    "count as territory for its captor once it is off the board" in {
      areaScoreOf(playing(Go9x9, List("a1", "b1", "pass", "a2")).board) === Score(0, 865)
    }
  }

  "a dead stone that a settlement lifted off the board" should {
    "hand the region it used to touch to the player who enclosed it" in {
      areaScoreOf(situationFrom(blackWallAfterTheDeadStoneCameOff).board) === Score(810, 55)
    }
  }

  "the dame between two walls that share every liberty" should {
    "score for nobody, with no special handling for the seki" in {
      areaScoreOf(situationFrom(wallsSharingEveryDamePointUnderNoKomi).board) === Score(360, 360)
    }
  }

  "a board carrying no komi at all" should {
    "leave the tie a tie, rather than handing p2 the komi its variant defaults to" in {
      val board = situationFrom(wallsSharingEveryDamePointUnderNoKomi).board
      (board.komi === 0.0) and
        (board.variant.komi === 5.5) and
        (areaScoreOf(board).p2 === areaScoreOf(board).p1)
    }
  }

  "a board carrying six point komi" should {
    "settle equal area into a tie, which the variant default of seven and a half would not" in {
      val board = situationFrom(equalAreaUnderSixPointKomi).board
      (board.komi === 6.0) and
        (board.variant.komi === 7.5) and
        (areaScoreOf(board) === Score(70, 70))
    }
  }

  "a board carrying six and a half point komi" should {
    "count stones plus enclosed territory, then add that board's komi to p2" in {
      val board = situationFrom(settledUnderSixAndAHalfPointKomi).board
      (board.komi === 6.5) and
        (board.variant.komi === 7.5) and
        (areaScoreOf(board) === Score(3220, 445))
    }
  }

  "a position that is decided but still has legal drops" should {
    "score the whole board to black with two points left to play" in {
      areaScoreOf(situationFrom(decidedButTwoPointsLeft).board) === Score(3610, 75)
    }
    "score 323 to 45.5 with twenty eight points left to play" in {
      areaScoreOf(situationFrom(decidedButTwentyEightPointsLeft).board) === Score(3230, 455)
    }
    "leave the neutral points out, scoring 322 to 45.5 with thirty eight left to play" in {
      areaScoreOf(situationFrom(decidedButThirtyEightPointsLeft).board) === Score(3220, 455)
    }
  }
}

object GoAreaScoreDifferentialTest {

  private val mismatchReportLimit = 25

  lazy val mismatches: List[String] = GoOracle.load().flatMap(mismatchesOf)

  private def areaScoreOf(board: Board): Score = board.variant.areaScore(board)

  private def mismatchesOf(recording: GoOracleGame): List[String] = {
    val replayed = replayedPlies(recording)
    if (replayed.size != recording.plies.size)
      List(s"${recording.name}: recorded ${recording.plies.size} plies, replayed ${replayed.size}")
    else
      recording.plies.zip(replayed).zipWithIndex.flatMap { case ((recorded, played), ply) =>
        mismatchedScores(s"${recording.name} ply ${ply}", recorded, played)
      }
  }

  private def mismatchedScores(named: String, recorded: GoOraclePly, played: Game): List[String] = {
    val scored = areaScoreOf(played.situation.board)
    List(
      mismatched(named, "the fen", scored, scoreInFen(fenOfRecord(recorded, played))),
      scoreRecordedFor(recorded).flatMap(mismatched(named, "the history", scored, _))
    ).flatten
  }

  private def fenOfRecord(recorded: GoOraclePly, played: Game): FEN =
    if (recorded.fen.nonEmpty) FEN(recorded.fen) else Forsyth.>>(played)

  private def scoreInFen(fen: FEN): Score = Score(fen.player1Score, fen.player2Score)

  private def scoreRecordedFor(recorded: GoOraclePly): Option[Score] =
    Some(Score(recorded.scoreP1, recorded.scoreP2)).filter(_.nonEmpty)

  private def mismatched(named: String, source: String, scored: Score, expected: Score): Option[String] =
    if (scored == expected) None
    else Some(s"${named}: areaScore ${scored.fenStr}, ${source} ${expected.fenStr}")

  private def replayedPlies(recording: GoOracleGame): List[Game] = {
    val variant                     = Variant(recording.variantKey)
      .getOrElse(sys.error(s"unknown go variant: ${recording.variantKey}"))
    val initialFen                  = recording.initialFen.fold(variant.initialFen)(FEN(_))
    val startPlayer                 = initialFen.player.getOrElse(Player.P1)
    val (opening, playedWithUci, _) = Replay.gameWithUciWhileValid(
      recording.actionStrs.map(Vector(_)).toVector,
      startPlayer,
      Player.fromTurnCount(recording.actionStrs.size + startPlayer.fold(0, 1)),
      initialFen,
      variant
    )
    opening :: playedWithUci.map { case (played, _) => played }
  }
}
