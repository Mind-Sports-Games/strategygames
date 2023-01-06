package strategygames

import strategygames.fairysf.Pos._

class ClockByoyomiTest extends ShogiTest {
  // TODO: test it with other games, not just shogi?
  val fakeClock60 = ByoyomiClock(60, 0, 0, 0)
    .copy(timestamper = new Timestamper {
      val now = Timestamp(0)
    })
    .start

  val fakeClock600 = ByoyomiClock(600, 0, 0, 0)
    .copy(timestamper = new Timestamper {
      val now = Timestamp(0)
    })
    .start

  val fakeClockByo = ByoyomiClock(15, 0, 5, 1)
    .copy(timestamper = new Timestamper {
      val now = Timestamp(0)
    })
    .start

  val fakeClockZero = ByoyomiClock(0, 0, 10, 1)
    .copy(timestamper = new Timestamper {
      val now = Timestamp(0)
    })
    .start

  val fakeClockPeriods = ByoyomiClock(10, 0, 10, 3)
    .copy(timestamper = new Timestamper {
      val now = Timestamp(0)
    })
    .start

  def advance(c: Clock, t: Int) =
    c.withTimestamper(new Timestamper {
      val now = c.timestamper.now + Centis(t)
    })

  "play with a clock" should {
    val clock = ByoyomiClock(5 * 60 * 1000, 0, 0, 0)
    val game  = makeGame.copy(clock=Some(clock.start))
    "new game" in {
      game.clock map { _.player } must_== Some(Player.P1)
    }
    "one move played" in {
      game.playMoves((C3, C4, None)) must beValid.like { case g =>
        g.clock map { _.player } must_== Some(Player.P2)
      }
    }
  }

  "create a clock" should {
    "with time" in {
      ByoyomiClock(60, 10, 0, 0).limitSeconds must_== 60
    }
    "with increment" in {
      ByoyomiClock(60, 10, 0, 0).incrementSeconds must_== 10
    }
    "with few time" in {
      ByoyomiClock(0, 10, 0, 0).limitSeconds must_== 0
    }
    "with 30 seconds" in {
      ByoyomiClock(30, 0, 0, 0).limitInMinutes must_== 0.5
    }
    "with time and byo" in {
      val c = ByoyomiClock(30, 0, 10, 1)
      c.limitInMinutes must_== 0.5
      c.spentPeriodsOf(Player.P1) must_== 0
      c.spentPeriodsOf(Player.P2) must_== 0
    }
    "without limit, only byo" in {
      val c = ByoyomiClock(0, 0, 10, 1)
      c.spentPeriodsOf(Player.P1) must_== 1
      c.spentPeriodsOf(Player.P2) must_== 1
    }
  }
  "lag compensation" should {
    def durOf(lag: Int) = MoveMetrics(clientLag = Some(Centis(lag)))

    def clockStep(clock: Clock, wait: Int, lags: Int*) = {
      (lags.foldLeft(clock) { (clk, lag) =>
        advance(clk.step(), wait + lag) step durOf(lag)
      } currentClockFor Player.P2).time.centis
    }

    def clockStep60(w: Int, l: Int*)  = clockStep(fakeClock60, w, l: _*)
    def clockStep600(w: Int, l: Int*) = clockStep(fakeClock600, w, l: _*)

    def clockStart(lag: Int) = {
      val clock = fakeClock60.step()
      ((clock step durOf(lag)) currentClockFor Player.P1).time.centis
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
      }
      "no -> medium lag" in {
        clockStep60(0, 0, 300) must_== 5940
      }
      "no x4 -> big lag" in {
        clockStep60(0, 0, 0, 0, 0, 700) must_== 5720
      }
    }
  }

  "live time checks" in {
    "60s stall" in {
      val clock60 = advance(fakeClock60, 60 * 100)
      val cc      = clock60.currentClockFor(Player.P1)

      cc.time.centis must_== 0
      cc.periods must_== 0
      clock60.outOfTime(Player.P2, withGrace = true) must beFalse
      clock60.outOfTime(Player.P1, withGrace = true) must beFalse
      clock60.outOfTime(Player.P1, withGrace = false) must beTrue
    }
    "61s stall" in {
      val clock61 = advance(fakeClock60, 61 * 100)
      val cc      = clock61.currentClockFor(Player.P1)

      cc.time.centis must_== 0
      cc.periods must_== 0
      clock61.outOfTime(Player.P1, withGrace = true) must beFalse

      advance(fakeClock60, 63 * 100).outOfTime(Player.P1, withGrace = true) must beTrue
    }
    "byoyomi clock before entering byoyomi" in {
      val clock10 = advance(fakeClockByo, 10 * 100)
      val cc      = clock10.currentClockFor(Player.P1)

      cc.time.centis must_== 5 * 100
      cc.periods must_== 0
      clock10.outOfTime(Player.P2, withGrace = false) must beFalse
      clock10.outOfTime(Player.P1, withGrace = false) must beFalse
    }
    "entering byoyomi, still having byo time" in {
      val clock17 = advance(fakeClockByo, 19 * 100)
      val cc      = clock17.currentClockFor(Player.P1)

      cc.time.centis must_== 1 * 100
      cc.periods must_== 1
      clock17.outOfTime(Player.P1, withGrace = false) must beFalse
    }
    "entering byoyomi, not having byo time" in {
      val clock20 = advance(fakeClockByo, 20 * 100)
      val cc      = clock20.currentClockFor(Player.P1)

      cc.time.centis must_== 0
      cc.periods must_== 1
      clock20.outOfTime(Player.P1, withGrace = false) must beTrue
    }
    "10s stall for zero clock with byo" in {
      val clock10 = advance(fakeClockZero, 10 * 100)
      val cc      = clock10.currentClockFor(Player.P1)

      cc.time.centis must_== 0
      cc.periods must_== 1
      clock10.outOfTime(Player.P1, withGrace = true) must beFalse
      clock10.outOfTime(Player.P1, withGrace = false) must beTrue
    }
    "11s stall for zero clock with byo" in {
      val clock11 = advance(fakeClockZero, 11 * 100)

      clock11.currentClockFor(Player.P1).time.centis must_== 0
      clock11.outOfTime(Player.P1, withGrace = true) must beFalse
    }
    "spanning over multiple periods" in {
      val clockPers = advance(fakeClockPeriods, 32 * 100)
      val cc        = clockPers.currentClockFor(Player.P1)

      cc.time.centis must_== 8 * 100
      cc.periods must_== 3
      clockPers.outOfTime(Player.P1, withGrace = false) must beFalse
    }

    "over quota stall" >> advance(fakeClock60, 6190).outOfTime(Player.P1, true)
    "stall within quota" >> !advance(fakeClock600, 60190).outOfTime(Player.P1, true)
    "max grace stall" >> advance(fakeClock600, 602 * 100).outOfTime(Player.P1, true)
  }

  "kif config" in {
    "everything" in {
      ByoyomiClock.readKifConfig("10分|20秒(1)+0秒") must_== Some(ByoyomiClock.Config(600, 0, 20, 1))
    }
    "without inc" in {
      ByoyomiClock.readKifConfig("10分|20秒(1)") must_== Some(ByoyomiClock.Config(600, 0, 20, 1))
    }
    "without per" in {
      ByoyomiClock.readKifConfig("10分|20秒+10秒") must_== Some(ByoyomiClock.Config(600, 10, 20, 1))
    }
    "without per and inc" in {
      ByoyomiClock.readKifConfig("10分|20秒") must_== Some(ByoyomiClock.Config(600, 0, 20, 1))
    }
    "without per and inc" in {
      ByoyomiClock.readKifConfig("10分+20秒") must_== Some(ByoyomiClock.Config(600, 0, 20, 1))
    }
    "mix of mins and secs" in {
      ByoyomiClock.readKifConfig("10分20秒") must_== Some(ByoyomiClock.Config(620, 0, 0, 1))
    }
    "hours" in {
      ByoyomiClock.readKifConfig("1時間+20秒") must_== Some(ByoyomiClock.Config(3600, 0, 20, 1))
    }
    "mix of hours mins and secs" in {
      ByoyomiClock.readKifConfig("1時間10分20秒+20秒") must_== Some(ByoyomiClock.Config(4220, 0, 20, 1))
    }
  }

  "show" in {
    ByoyomiClock.Config(600, 0, 20, 1).show must_== "10|20"
    ByoyomiClock.Config(300, 0, 20, 2).show must_== "5|20(2x)"
    ByoyomiClock.Config(300, 10, 20, 2).show must_== "5+10|20(2x)"
    ByoyomiClock.Config(60, 10, 0, 1).show must_== "1+10"
    ByoyomiClock.Config(0, 10, 0, 1).show must_== "0+10"
    ByoyomiClock.Config(0, 0, 10, 1).show must_== "0|10"
    ByoyomiClock.Config(600, 0, 0, 1).show must_== "10|0"
  }
}
