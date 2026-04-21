package strategygames.fairysf

import strategygames.{ ByoyomiClock, P1 }

class BerserkByoyomiTest extends ShogiTest {

  def senteBerserkLimit(minutes: Int, seconds: Int, byo: Int) =
    ByoyomiClock(minutes * 60, seconds, byo, 1).goBerserk(P1).currentClockFor(P1).time.centis * .01

  def senteBerserkByoyomi(minutes: Int, seconds: Int, byo: Int) =
    ByoyomiClock(minutes * 60, seconds, byo, 1).goBerserk(P1).byoyomiOf(P1).centis * .01

  def senteBerserkGoteByoyomi(minutes: Int, seconds: Int, byo: Int) =
    ByoyomiClock(minutes * 60, seconds, byo, 1).goBerserk(P1).byoyomiOf(P2).centis * .01

  def senteBerserkPeriods(minutes: Int, seconds: Int, byo: Int) =
    ByoyomiClock(minutes * 60, seconds, byo, 1).goBerserk(P1).periodsTotal

  "berserkable" should {
    "yep" in {
      ByoyomiClock.Config(60 * 60, 0, 0, 1).berserkable === true
      ByoyomiClock.Config(1 * 60, 0, 0, 1).berserkable === true
      ByoyomiClock.Config(60 * 60, 60, 0, 1).berserkable === true
      ByoyomiClock.Config(60 * 60, 0, 60, 1).berserkable === true
      ByoyomiClock.Config(1 * 60, 0, 0, 1).berserkable === true
    }
    "nope" in {
      ByoyomiClock.Config(0 * 60, 1, 0, 1).berserkable === false
      ByoyomiClock.Config(0 * 60, 10, 0, 1).berserkable === false
      ByoyomiClock.Config(0 * 60, 0, 10, 1).berserkable === false
    }
  }
  "berserk flags" should {
    "P1" in {
      ByoyomiClock(60, 0, 0, 1).berserked(P1) === false
      ByoyomiClock(60, 0, 0, 1).goBerserk(P1).berserked(P1) === true
    }
    "gote" in {
      ByoyomiClock(60, 0, 0, 1).berserked(P2) === false
      ByoyomiClock(60, 0, 0, 1).goBerserk(P2).berserked(P2) === true
    }
  }
  "initial time penalty, no byoyomi, no increment" should {
    "10+0" in {
      senteBerserkLimit(10, 0, 0) === 5 * 60
    }
    "5+0" in {
      senteBerserkLimit(5, 0, 0) === 2.5 * 60
    }
    "3+0" in {
      senteBerserkLimit(3, 0, 0) === 1.5 * 60
    }
    "1+0" in {
      senteBerserkLimit(1, 0, 0) === 0.5 * 60
    }
  }
  "initial time penalty, no byoyomi, with increment" should {
    "4+4" in {
      senteBerserkLimit(4, 4, 0) === 2 * 60
    }
    "3+2" in {
      senteBerserkLimit(3, 2, 0) === 1.5 * 60
    }
    "2+10" in {
      senteBerserkLimit(2, 10, 0) === 2 * 60
    }
    "10+5" in {
      senteBerserkLimit(10, 5, 0) === 5 * 60
    }
    "10+2" in {
      senteBerserkLimit(10, 2, 0) === 5 * 60
    }
    "1+1" in {
      senteBerserkLimit(1, 1, 0) === 0.5 * 60
    }
    "1+3" in {
      senteBerserkLimit(1, 3, 0) === 1 * 60
    }
    "1+5" in {
      senteBerserkLimit(1, 5, 0) === 1 * 60
    }
  }
  "initial time penalty, with byoyomi, no increment" should {
    "3|2" in {
      senteBerserkLimit(3, 0, 2) === 1.5 * 60
      senteBerserkByoyomi(3, 0, 2) === 1
      senteBerserkGoteByoyomi(3, 0, 2) === 2
      senteBerserkPeriods(3, 0, 2) === 1
    }
  }
}
