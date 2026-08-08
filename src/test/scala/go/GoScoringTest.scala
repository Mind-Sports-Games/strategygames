package strategygames.go

import org.specs2.mutable.Specification

import strategygames.Player
import strategygames.go.format.FEN
import strategygames.go.variant.{ Go13x13, Go19x19, Go9x9, Variant }

class GoScoringTest extends Specification with GoRulesTestSupport {

  private val blackWallWithOneWhiteStone =
    FEN(s"4S4/4S4/4S4/4S4/4S1s2/4S4/4S4/4S4/4S4${pocket} b - 450 65 0 0 55 2 1")

  private val singlePointEye = FEN(s"8s/9/9/9/9/9/9/S8/1S7${pocket} b - 30 65 0 0 55 0 1")

  private val wallsSharingEveryDamePoint =
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

  private val settledNineteenByNineteen = FEN(
    s"${(List("s1" * 9 + "s", "s" * 19, "1" + "S" * 18) ++
        List.fill(10)("S" * 19) ++
        List("S1" * 9 + "S", "S" * 19, "S1" * 9 + "S", "S" * 19, "S" * 19, "19")).mkString("/")}${pocket} b - 3220 445 0 0 65 0 1"
  )

  private val komiTenthsByVariant = List[(Variant, Int)]((Go9x9, 55), (Go13x13, 75), (Go19x19, 75))

  "an empty board" should {
    "give neither player any area, leaving p2 holding nothing but komi" in {
      forall(komiTenthsByVariant) { case (variant, komiTenths) =>
        val opening = fenOf(Game(variant))
        (opening.player1Score === 0) and (opening.player2Score === komiTenths)
      }
    }
  }

  "komi" should {
    "be carried in the fen as tenths of a point" in {
      forall(komiTenthsByVariant) { case (variant, komiTenths) =>
        val opening = fenOf(Game(variant))
        (opening.komi === komiTenths / 10.0) and
          (opening.value.split(' ')(7) === komiTenths.toString)
      }
    }
    "be half a point smaller on the smallest board" in {
      (Go9x9.komi === 5.5) and (Go13x13.komi === 7.5) and (Go19x19.komi === 7.5)
    }
  }

  "a board holding one black stone and nothing else" should {
    "give black every point of it" in {
      val scored = fenOf(playing(Go9x9, List("e5")))
      (scored.player1Score === 810) and (scored.player2Score === 55)
    }
  }

  "a region that both colours touch" should {
    "belong to nobody, leaving each player only the stone they played" in {
      val scored = fenOf(playing(Go9x9, List("a1", "b1")))
      (scored.player1Score === 10) and (scored.player2Score === 65)
    }
  }

  "the dame between two walls that share every liberty" should {
    "score for nobody, with no special handling for the seki" in {
      val scored = fenOf(situationFrom(wallsSharingEveryDamePoint))
      (scored.player1Score === 360) and (scored.player2Score === 360)
    }
    "leave a settled game with equal area and no komi without a winner" in {
      val settled = playingFrom(wallsSharingEveryDamePoint, List("pass", "pass", "ss:"))
      val scored  = fenOf(settled)
      (scored.player1Score === 360) and
        (scored.player2Score === 360) and
        (settled.situation.end === true) and
        (settled.situation.winner === None)
    }
  }

  "a settled 19x19 game whose komi does not break the tie" should {
    "have no winner either" in {
      val settled = playingFrom(equalAreaUnderSixPointKomi, List("pass", "pass", "ss:"))
      val scored  = fenOf(settled)
      (scored.player1Score === scored.player2Score) and
        (settled.situation.end === true) and
        (settled.situation.winner === None)
    }
  }

  "a region enclosed against black that only white touches" should {
    "belong to nobody either" in {
      val scored = fenOf(situationFrom(blackWallWithOneWhiteStone))
      (scored.player1Score === 450) and (scored.player2Score === 65)
    }
  }

  "a single point eye" should {
    "belong to the player enclosing it" in {
      val scored = fenOf(situationFrom(singlePointEye))
      (scored.player1Score === 30) and (scored.player2Score === 65)
    }
  }

  "removing a dead stone" should {
    val settled = playingFrom(blackWallWithOneWhiteStone, List("ss:g5"))
    "hand its surrounded region to the opponent" in {
      val scored = fenOf(settled)
      (scored.player1Score === 810) and (scored.player2Score === 55)
    }
    "decide the game for the player who enclosed it" in {
      (settled.situation.end === true) and (settled.situation.winner === Some(Player.P1))
    }
  }

  "a stone that was captured" should {
    "count as territory for its captor once it is off the board" in {
      val scored = fenOf(playing(Go9x9, List("a1", "b1", "pass", "a2")))
      (scored.player1Score === 0) and (scored.player2Score === 865)
    }
  }

  "a position that is decided but still has legal drops" should {
    "score the whole board to black with two points left to play" in {
      val decided = situationFrom(decidedButTwoPointsLeft)
      (dropKeysOf(decided).size === 2) and
        (fenOf(decided).player1Score === 3610) and
        (fenOf(decided).player2Score === 75) and
        (decided.end === false)
    }
    "score 323 to 45.5 with twenty eight points left to play" in {
      val decided = situationFrom(decidedButTwentyEightPointsLeft)
      (dropKeysOf(decided).size === 28) and
        (fenOf(decided).player1Score === 3230) and
        (fenOf(decided).player2Score === 455) and
        (decided.end === false)
    }
    "leave the neutral points out, scoring 322 to 45.5 with thirty eight left to play" in {
      val decided = situationFrom(decidedButThirtyEightPointsLeft)
      (dropKeysOf(decided).size === 38) and
        (fenOf(decided).player1Score === 3220) and
        (fenOf(decided).player2Score === 455) and
        (decided.end === false)
    }
  }

  "a settled 19x19 position" should {
    "count stones plus enclosed territory for each player, then add komi to p2" in {
      val scored = fenOf(situationFrom(settledNineteenByNineteen))
      (scored.player1Score === 3220) and (scored.player2Score === 445) and (scored.komi === 6.5)
    }
  }
}
