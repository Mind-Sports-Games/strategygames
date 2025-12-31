package strategygames.chess

import strategygames._

class ClockZeroInitialTimeTest extends ChessTest {

  def advanceTimeByCentis(c: ClockBase, t: Int) =
    c.withTimestamper(new Timestamper {
      val now = c.timestamper.now + Centis(t)
    })

  def seconds(s: Int): Centis = Centis(s * 100)

  def makeFakeClock(config: Clock.Config) =
    Clock(config)
      .copy(timestamper = new Timestamper {
        val now = Timestamp(0)
      })
      .start

  def makeFakeClockBronstein(config: Clock.BronsteinConfig) =
    Clock(config)
      .copy(timestamper = new Timestamper {
        val now = Timestamp(0)
      })
      .start

  def makeFakeClockSimpleDelay(config: Clock.SimpleDelayConfig) =
    Clock(config)
      .copy(timestamper = new Timestamper {
        val now = Timestamp(0)
      })
      .start

  // Simulates taking time and ending turn (single action per turn)
  def takeTimeAndEndTurn(clock: ClockBase, timeTaken: Centis): ClockBase = {
    val afterAction = advanceTimeByCentis(clock, timeTaken.centis).recordActionTime()
    afterAction.endTurn
  }

  // ==========================================================================
  // Fischer Increment: 0+10
  // Expected behavior:
  // - Initial time: increment (10s), with minimum of 3 seconds
  // - First move: clock doesn't count down, still have full 10s after
  // - First move: NO increment given
  // - Subsequent moves: time counts down normally, increment given
  // ==========================================================================
  "Fischer increment with 0 initial time (0+10)" should {
    val config = Clock.Config(limitSeconds = 0, incrementSeconds = 10)

    "start with increment as initial time" in {
      val clock = makeFakeClock(config)
      clock.remainingTime(P1) must_== seconds(10)
      clock.remainingTime(P2) must_== seconds(10)
    }

    "not count down on first move and not give increment (P1's first turn)" in {
      val clock   = makeFakeClock(config)
      val afterP1 = takeTimeAndEndTurn(clock, seconds(0))
      afterP1.remainingTime(P1) must_== seconds(10)
    }

    "not count down on first move and not give increment (P2's first turn)" in {
      val clock   = makeFakeClock(config)
      val afterP1 = takeTimeAndEndTurn(clock, seconds(0))
      val afterP2 = takeTimeAndEndTurn(afterP1, seconds(0))
      afterP2.remainingTime(P2) must_== seconds(10)
    }

    "count down and give increment on second move" in {
      val clock     = makeFakeClock(config)
      val afterP1T1 = takeTimeAndEndTurn(clock, seconds(0))
      val afterP2T1 = takeTimeAndEndTurn(afterP1T1, seconds(0))
      val afterP1T2 = takeTimeAndEndTurn(afterP2T1, seconds(4))
      afterP1T2.remainingTime(P1) must_== seconds(16)
    }
  }

  // ==========================================================================
  // Simple Delay (US Delay): 0 d/10
  // Expected behavior:
  // - Initial display: delay + 1 centis (boundary buffer)
  // - First move: doesn't count down
  // - After subsequent moves: delay gives you X seconds buffer per move
  // ==========================================================================
  "Simple delay with 0 initial time (0 d/10)" should {
    val config = Clock.SimpleDelayConfig(limitSeconds = 0, delaySeconds = 10)
    val expectedTime = seconds(10) + Centis(1)

    "start with delay + 1 centis as displayed time" in {
      val clock = makeFakeClockSimpleDelay(config)
      clock.remainingTime(P1) must_== expectedTime
      clock.remainingTime(P2) must_== expectedTime
    }

    "not count down on first move (P1)" in {
      val clock   = makeFakeClockSimpleDelay(config)
      val afterP1 = takeTimeAndEndTurn(clock, seconds(0))
      afterP1.remainingTime(P1) must_== expectedTime
    }

    "not count down on first move (P2)" in {
      val clock   = makeFakeClockSimpleDelay(config)
      val afterP1 = takeTimeAndEndTurn(clock, seconds(0))
      val afterP2 = takeTimeAndEndTurn(afterP1, seconds(0))
      afterP2.remainingTime(P2) must_== expectedTime
    }

    "have full delay on second move within delay time" in {
      val clock     = makeFakeClockSimpleDelay(config)
      val afterP1T1 = takeTimeAndEndTurn(clock, seconds(0))
      val afterP2T1 = takeTimeAndEndTurn(afterP1T1, seconds(0))
      val afterP1T2 = takeTimeAndEndTurn(afterP2T1, seconds(6))
      afterP1T2.remainingTime(P1) must_== expectedTime
    }

    "lose time on second move when exceeding delay" in {
      val clock     = makeFakeClockSimpleDelay(config)
      val afterP1T1 = takeTimeAndEndTurn(clock, seconds(0))
      val afterP2T1 = takeTimeAndEndTurn(afterP1T1, seconds(0))
      val afterP1T2 = takeTimeAndEndTurn(afterP2T1, seconds(12))
      afterP1T2.outOfTime(P1, withGrace = false) must beTrue
    }
  }

  // ==========================================================================
  // Bronstein Delay: 0 d+10
  // Expected behavior:
  // - Initial display: delay amount (10s) as starting time
  // - First move: doesn't count down
  // - After subsequent moves: get back what you used (up to delay)
  // ==========================================================================
  "Bronstein delay with 0 initial time (0 d+10)" should {
    val config = Clock.BronsteinConfig(limitSeconds = 0, delaySeconds = 10)

    "start with delay as initial time" in {
      val clock = makeFakeClockBronstein(config)
      clock.remainingTime(P1) must_== seconds(10)
      clock.remainingTime(P2) must_== seconds(10)
    }

    "not count down on first move (P1)" in {
      val clock   = makeFakeClockBronstein(config)
      val afterP1 = takeTimeAndEndTurn(clock, seconds(0))
      afterP1.remainingTime(P1) must_== seconds(10)
    }

    "not count down on first move (P2)" in {
      val clock   = makeFakeClockBronstein(config)
      val afterP1 = takeTimeAndEndTurn(clock, seconds(0))
      val afterP2 = takeTimeAndEndTurn(afterP1, seconds(0))
      afterP2.remainingTime(P2) must_== seconds(10)
    }

    "give back time used on second move" in {
      val clock     = makeFakeClockBronstein(config)
      val afterP1T1 = takeTimeAndEndTurn(clock, seconds(0))
      val afterP2T1 = takeTimeAndEndTurn(afterP1T1, seconds(0))
      val afterP1T2 = takeTimeAndEndTurn(afterP2T1, seconds(6))
      afterP1T2.remainingTime(P1) must_== seconds(10)
    }

    "cap giveback at delay amount" in {
      val clock     = makeFakeClockBronstein(config)
      val afterP1T1 = takeTimeAndEndTurn(clock, seconds(0))
      val afterP2T1 = takeTimeAndEndTurn(afterP1T1, seconds(0))
      val afterP1T2 = takeTimeAndEndTurn(afterP2T1, seconds(8))
      afterP1T2.remainingTime(P1) must_== seconds(10)
    }

    "timeout on second move when using all time" in {
      val clock     = makeFakeClockBronstein(config)
      val afterP1T1 = takeTimeAndEndTurn(clock, seconds(0))
      val afterP2T1 = takeTimeAndEndTurn(afterP1T1, seconds(0))
      val afterP1T2 = takeTimeAndEndTurn(afterP2T1, seconds(10))
      afterP1T2.remainingTime(P1) must_== seconds(0)
      afterP1T2.outOfTime(P1, withGrace = false) must beTrue
    }
  }

  // ==========================================================================
  // Comparison: Non-zero initial time should work normally (time counts from turn 1)
  // ==========================================================================
  "Fischer increment with positive initial time (5+10)" should {
    val config = Clock.Config(limitSeconds = 5, incrementSeconds = 10)

    "count down and give increment on first move" in {
      val clock   = makeFakeClock(config)
      val afterP1 = takeTimeAndEndTurn(clock, seconds(0))
      afterP1.remainingTime(P1) must_== seconds(5)
    }
  }
}
