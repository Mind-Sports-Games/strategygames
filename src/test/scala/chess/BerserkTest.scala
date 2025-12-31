package strategygames.chess
import strategygames.{ Clock, GameLogic, P1, P2 }

class BerserkTest extends ChessTest {

  val chess = GameLogic.Chess()

  def p1Berserk(minutes: Int, seconds: Int) =
    Clock(Clock.Config(minutes * 60, seconds)).goBerserk(P1).remainingTime(P1).centis * .01

  "berserkable" should {
    "yep" in {
      Clock.Config(60 * 60, 0).berserkable === true
      Clock.Config(1 * 60, 0).berserkable === true
      Clock.Config(60 * 60, 60).berserkable === true
      Clock.Config(1 * 60, 0).berserkable === true
    }
    "nope" in {
      Clock.Config(0 * 60, 1).berserkable === false
      Clock.Config(0 * 60, 10).berserkable === false
    }
  }
  "berserk flags" should {
    "p1" in {
      Clock(Clock.Config(60, 0)).berserked(P1) === false
      Clock(Clock.Config(60, 0)).goBerserk(P1).berserked(P1) === true
    }
    "p2" in {
      Clock(Clock.Config(60, 0)).berserked(P2) === false
      Clock(Clock.Config(60, 0)).goBerserk(P2).berserked(P2) === true
    }
  }
  "initial time penalty, no increment" should {
    "10+0" in {
      p1Berserk(10, 0) === 5 * 60
    }
    "5+0" in {
      p1Berserk(5, 0) === 2.5 * 60
    }
    "3+0" in {
      p1Berserk(3, 0) === 1.5 * 60
    }
    "1+0" in {
      p1Berserk(1, 0) === 0.5 * 60
    }
  }
  "initial time penalty, with increment" should {
    "4+4" in {
      p1Berserk(4, 4) === 2 * 60
    }
    "3+2" in {
      p1Berserk(3, 2) === 1.5 * 60
    }
    "2+10" in {
      p1Berserk(2, 10) === 2 * 60
    }
    "10+5" in {
      p1Berserk(10, 5) === 5 * 60
    }
    "10+2" in {
      p1Berserk(10, 2) === 5 * 60
    }
    "1+1" in {
      p1Berserk(1, 1) === 0.5 * 60
    }
    "1+3" in {
      p1Berserk(1, 3) === 1 * 60
    }
    "1+5" in {
      p1Berserk(1, 5) === 1 * 60
    }
  }
}
