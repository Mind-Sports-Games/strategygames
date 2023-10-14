package strategygames.chess
import strategygames._

import strategygames.chess.Pos._

class TimerTest extends ChessTest {
  "play with a fischer increment" should {
    val fischerIncrement = Timer
      .fischerIncrement(Centis(60 * 100), Centis(1 * 100))
    "properly increment time when game is ongoing" in {
      fischerIncrement.remaining must_== Centis(60 * 100)
      val afterMove = fischerIncrement
        .takeTime(Centis(30 * 100))
      afterMove.remaining must_== Centis(31 * 100)
    }
    "even when there is only 1 centisecond left" in {
      val afterMove = fischerIncrement
        .takeTime(Centis(30 * 100))
        .takeTime(Centis(31 * 100 - 1))
      afterMove.remaining must_== Centis(101)
      afterMove.outOfTime must_== false
    }
    "but not when game is over" in {
      val afterMove = fischerIncrement
        .takeTime(Centis(30 * 100))
        .takeTime(Centis(31 * 100))
      afterMove.remaining must_== Centis(0 * 100)
      afterMove.outOfTime must_== true
    }
  }

  "play with a bronstein delay increment" should {
    val bronsteinDelay = Timer.bronsteinDelay(Centis(60 * 100), Centis(5 * 100))
    "properly increment time when game is ongoing" in {
      bronsteinDelay.remaining must_== Centis(60 * 100)
      val afterMove = bronsteinDelay
        .takeTime(Centis(30 * 100))
      afterMove.remaining must_== Centis(35 * 100)
    }
    "even when there is only 1 centisecond left" in {
      val afterMove = bronsteinDelay
        .takeTime(Centis(30 * 100))
        .takeTime(Centis(35 * 100 - 1))
      afterMove.remaining must_== Centis(501)
      afterMove.outOfTime must_== false
    }
    "and also when game is over and the delay would save it" in {
      val afterMove = bronsteinDelay
        .takeTime(Centis(30 * 100))
        .takeTime(Centis(36 * 100))
      afterMove.remaining must_== Centis(4 * 100)
      afterMove.outOfTime must_== false
    }
    "but now when the delay + move still doesn't save it." in {
      val afterMove = bronsteinDelay
        .takeTime(Centis(30 * 100))
        .takeTime(Centis(40 * 100))
      afterMove.remaining must_== Centis(0)
      afterMove.outOfTime must_== true
    }
  }

  // Byoyomi can be represented multiple different ways, but it's basically a new timer
  // without increment after the current period.
  "play with a fischer increment followed by byoyomi" should {
    val withByoyomi    =
      Timer
        .fischerIncrement(Centis(60 * 100), Centis(1 * 100))
        .followedBy(Timer.byoyomi(Centis(5 * 100)))
    val withTwoByoyomi =
      Timer
        .fischerIncrement(Centis(60 * 100), Centis(1 * 100))
        .followedBy(Timer.byoyomi(Centis(5 * 100)))
        .followedBy(Timer.byoyomi(Centis(5 * 100)))
    "properly increment time when game is ongoing" in {
      withByoyomi.remaining must_== Centis(60 * 100)
      val afterMove = withByoyomi.takeTime(Centis(30 * 100))
      afterMove.remaining must_== Centis(31 * 100)
    }
    "even when there is only 1 centisecond left" in {
      val afterMove = withByoyomi
        .takeTime(Centis(30 * 100))
        .takeTime(Centis(31 * 100 - 1))
      afterMove.remaining must_== Centis(101)
      afterMove.outOfTime must_== false
    }
    "but when the current period ends we must get a new period of time to play with" in {
      val afterMove = withByoyomi
        .takeTime(Centis(30 * 100))
        .takeTime(Centis(31 * 100))
      afterMove.remaining must_== Centis(5 * 100)
      afterMove.outOfTime must_== false
    }
    "and so long as we continue to use less than the byoyomi, we get it back" in {
      val afterMove = withByoyomi
        .takeTime(Centis(30 * 100)) // fine
        .takeTime(Centis(31 * 100)) // period over
        .takeTime(Centis(4 * 100))  // fine
        .takeTime(Centis(4 * 100))  // fine
        .takeTime(Centis(4 * 100))  // fine
        .takeTime(Centis(4 * 100))  // fine
        .takeTime(Centis(4 * 100))  // fine
      afterMove.remaining must_== Centis(5 * 100)
      afterMove.outOfTime must_== false
    }
    "but if we go over, we don't" in {
      val afterMove = withByoyomi
        .takeTime(Centis(30 * 100)) // fine
        .takeTime(Centis(31 * 100)) // period over
        .takeTime(Centis(4 * 100))  // fine
        .takeTime(Centis(4 * 100))  // fine
        .takeTime(Centis(4 * 100))  // fine
        .takeTime(Centis(4 * 100))  // fine
        .takeTime(Centis(4 * 100))  // fine
        .takeTime(Centis(5 * 100))  // period over
      afterMove.remaining must_== Centis(0 * 100)
      afterMove.outOfTime must_== true
    }
    "with two periods we can go over twice" in {
      val afterMove = withTwoByoyomi
        .takeTime(Centis(30 * 100))
        .takeTime(Centis(31 * 100))
        .takeTime(Centis(4 * 100)) // fine
        .takeTime(Centis(4 * 100)) // fine
        .takeTime(Centis(4 * 100)) // fine
        .takeTime(Centis(5 * 100)) // period over
        .takeTime(Centis(4 * 100)) // fine
        .takeTime(Centis(4 * 100)) // fine
        .takeTime(Centis(4 * 100)) // fine
        .takeTime(Centis(4 * 100)) // fine
        .takeTime(Centis(4 * 100)) // fine
        .takeTime(Centis(5 * 100)) // period over
      afterMove.remaining must_== Centis(0 * 100)
      afterMove.outOfTime must_== true
    }
  }
}

class ClockTest extends ChessTest {
  val chess       = GameLogic.Chess()
  val fakeClock60 = Clock(Clock.Config(60, 0))
    .copy(timestamper = new Timestamper {
      val now = Timestamp(0)
    })
    .start

  val fakeClock600 = Clock(Clock.Config(600, 0))
    .copy(timestamper = new Timestamper {
      val now = Timestamp(0)
    })
    .start

  val fakeClock60Plus1 = Clock(Clock.Config(60, 1))
    .copy(timestamper = new Timestamper {
      val now = Timestamp(0)
    })
    .start

  val fakeClock180Delay2 = Clock(Clock.BronsteinConfig(180, 2))
    .copy(timestamper = new Timestamper {
      val now = Timestamp(0)
    })
    .start

  def advance(c: ClockBase, t: Int) =
    c.withTimestamper(new Timestamper {
      val now = c.timestamper.now + Centis(t)
    })

  "play with a clock" should {
    val clock = Clock(Clock.Config(5 * 60 * 1000, 0))
    val game  = makeGame.withClock(clock.start)
    "new game" in {
      game.clock map { _.player } must_== Option(P1)
    }
    "one move played" in {
      game.playMoves(E2 -> E4) must beValid.like { case g: strategygames.chess.Game =>
        g.clock map { _.player } must_== Option(P2)
      }
    }
  }
  "create a clock" should {
    "with time" in {
      Clock(Clock.Config(60, 10)).limitSeconds must_== 60
    }
    "with increment" in {
      Clock(Clock.Config(60, 10)).incrementSeconds must_== 10
    }
    "with few time" in {
      Clock(Clock.Config(0, 10)).limitSeconds must_== 0
    }
    "with 30 seconds" in {
      Clock(Clock.Config(30, 0)).limitInMinutes must_== 0.5
    }
  }
  "lag compensation" should {
    def durOf(lag: Int) = MoveMetrics(clientLag = Option(Centis(lag)))

    def clockStep(clock: ClockBase, wait: Int, lags: Int*) = {
      (lags
        .foldLeft(clock) { (clk, lag) =>
          advance(clk.step(), wait + lag).step(durOf(lag))
        }
        .remainingTime(P2))
        .centis
    }

    def clockStep60(w: Int, l: Int*)        = clockStep(fakeClock60, w, l: _*)
    def clockStep60Plus1(w: Int, l: Int*)   = clockStep(fakeClock60Plus1, w, l: _*)
    def clockStep600(w: Int, l: Int*)       = clockStep(fakeClock600, w, l: _*)
    def clockStep180Delay2(w: Int, l: Int*) = clockStep(fakeClock180Delay2, w, l: _*)

    def clockStart(lag: Int) = {
      val clock = fakeClock60.step()
      ((clock.step(durOf(lag))).remainingTime(P1)).centis
    }

    "start" in {
      "no lag" in {
        clockStart(0) must_== 60 * 100
      }
      "small lag" in {
        clockStart(20) must_== 60 * 100
      }
      "big lag" in {
        clockStart(500) must_== 60 * 100
      }
    }

    "1 move" in {
      "premove, no lag" in {
        clockStep600(0, 0) must_== 600 * 100
      }
      "premove, small lag" in {
        clockStep600(0, 20) must_== 600 * 100
      }
      "premove, big lag" in {
        clockStep600(0, 400) must_== 599 * 100
      }
      "1s move, no lag" in {
        clockStep600(100, 0) must_== 599 * 100
      }
      "1s move, small lag" in {
        clockStep600(100, 20) must_== 599 * 100
      }
      "1s move, big lag" in {
        clockStep600(100, 400) must_== 598 * 100
      }
    }

    "multiple premoves" in {
      "no lag" in {
        clockStep600(0, 0, 0) must_== 600 * 100
      }
      "medium lag x2" in {
        clockStep600(0, 300, 300) must_== 598 * 100
      }
      "no -> medium lag" in {
        clockStep600(0, 0, 300) must_== 600 * 100
      }
      "no x8 -> big lag" in {
        clockStep600(0, 0, 0, 0, 0, 0, 0, 0, 0, 800) must_== 599 * 100
      }

      "no x5 -> big lag x2" in {
        clockStep600(0, 0, 0, 0, 0, 0, 500, 600) must_== 597 * 100
      }

      "no x5 -> big lag x3" in {
        clockStep600(0, 0, 0, 0, 0, 0, 500, 500, 500) must_== 594 * 100
      }
    }

    "multiple premoves with fast clock" in {
      "no lag" in {
        clockStep60(0, 0, 0) must_== 60 * 100
        clockStep60Plus1(0, 0) must_== 61 * 100
        clockStep60Plus1(0, 0, 0) must_== 62 * 100
      }
      "no -> medium lag" in {
        clockStep60(0, 0, 300) must_== 5940
        clockStep60Plus1(0, 0, 300) must_== 6192
      }
      "no x4 -> big lag" in {
        clockStep60(0, 0, 0, 0, 0, 700) must_== 5720
        clockStep60Plus1(0, 0, 0, 0, 0, 700) must_== 6311
      }
    }

    "Basic clock behavior without lag" in {
      // Without increment our clock doesn't go up
      "60+0 3x 3s move" in {
        clockStep60(300, 0, 0, 0) must_== 51 * 100
      }
      "60+0 3x 2s move" in {
        clockStep60(200, 0, 0, 0) must_== 54 * 100
      }
      "60+0 3x 1s move" in {
        clockStep60(100, 0, 0, 0) must_== 57 * 100
      }
      "60+0 3x 0s move" in {
        clockStep60(0, 0, 0, 0) must_== 60 * 100
      }

      // With increment our clock can go up.
      "60+1 3x 3s move" in {
        clockStep60Plus1(300, 0, 0, 0) must_== 54 * 100
      }
      "60+1 3x 2s move" in {
        clockStep60Plus1(200, 0, 0, 0) must_== 57 * 100
      }
      "60+1 3x 1s move" in {
        clockStep60Plus1(100, 0, 0, 0) must_== 60 * 100
      }
      "60+1 3x 0s move" in {
        clockStep60Plus1(0, 0, 0, 0) must_== 63 * 100
      }

      // With delay our clock doesn't go up, but doesn't go down
      // either until we get to above the delay for our move time
      "180+2 3x 3s move" in {
        clockStep180Delay2(300, 0, 0, 0) must_== 177 * 100
      }
      "180+2 3x 2s move" in {
        clockStep180Delay2(200, 0, 0, 0) must_== 180 * 100
      }
      "180+2 3x 1s move" in {
        clockStep180Delay2(100, 0, 0, 0) must_== 180 * 100
      }
      "180+2 3x 0s move" in {
        clockStep180Delay2(0, 0, 0, 0) must_== 180 * 100
      }
    }
  }

  "live time checks" in {
    "60s stall" in {
      val clock60 = advance(fakeClock60, 60 * 100)

      clock60.remainingTime(P1).centis must_== 0
      clock60.outOfTime(P2, withGrace = true) must beFalse
      clock60.outOfTime(P1, withGrace = true) must beFalse
      clock60.outOfTime(P1, withGrace = false) must beTrue
    }
    "60s stall with increment" in {
      val clock = advance(fakeClock60Plus1, 60 * 100)

      clock.remainingTime(P1).centis must_== 0
      clock.outOfTime(P2, withGrace = true) must beFalse
      clock.outOfTime(P1, withGrace = true) must beFalse
      clock.outOfTime(P1, withGrace = false) must beTrue
    }
    "60s stall with byoyomi" in {
      val clock = advance(fakeClock180Delay2, 180 * 100)

      clock.remainingTime(P1).centis must_== 0
      clock.outOfTime(P2, withGrace = true) must beFalse
      clock.outOfTime(P1, withGrace = true) must beFalse
      clock.outOfTime(P1, withGrace = false) must beTrue
    }

    def clockStep(clock: ClockBase, wait: Int) =
      advance(clock, wait).step().step()

    "1x30s move + 30s stall with increment" in {
      val clockHalf = clockStep(fakeClock60Plus1, 30 * 100)
      clockHalf.remainingTime(P1).centis must_== 31 * 100
      val clock     = advance(clockHalf, 30 * 100)

      clock.remainingTime(P1).centis must_== 1 * 100
      clock.outOfTime(P2, withGrace = true) must beFalse
      clock.outOfTime(P1, withGrace = true) must beFalse
      clock.outOfTime(P1, withGrace = false) must beFalse
    }
    "1x100 move + 80s stall with byoyomi" in {
      val clockHalf = clockStep(fakeClock180Delay2, 100 * 100)
      clockHalf.remainingTime(P1).centis must_== 82 * 100
      val clock     = advance(clockHalf, 80 * 100)

      clock.remainingTime(P1).centis must_== 2 * 100
      clock.outOfTime(P2, withGrace = true) must beFalse
      clock.outOfTime(P1, withGrace = true) must beFalse
      clock.outOfTime(P1, withGrace = false) must beFalse
    }
    "61s stall" in {
      val clock61 = advance(fakeClock60, 61 * 100)
      clock61.remainingTime(P1).centis must_== 0
      clock61.outOfTime(P1, withGrace = true) must beFalse
    }
    "over quota stall" >> advance(fakeClock60, 6190).outOfTime(P1, withGrace = true)
    "stall within quota" >> !advance(fakeClock600, 60190).outOfTime(P1, withGrace = true)
    "max grace stall" >> advance(fakeClock600, 602 * 100).outOfTime(P1, withGrace = true)
  }

}
