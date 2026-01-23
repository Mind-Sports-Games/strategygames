package strategygames

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class ClockDisplayTest extends Specification with ValidatedMatchers {

  "Fischer show" in {
    Clock.Config(60, 0).show must_== "1+0"
    Clock.Config(120, 2).show must_== "2+2"
    Clock.Config(30, 10).show must_== "½+10"
    Clock.Config(0, 2).show must_== "0+2"
  }

  "Fischer showBerserk" in {
    Clock.Config(60, 0).showBerserk must_== "½+0"
    Clock.Config(120, 2).showBerserk must_== "1+0"
    Clock.Config(30, 10).showBerserk must_== "½+0"
    Clock.Config(0, 2).showBerserk must_== Clock.Config(0, 2).show
  }

  "Bronstein show" in {
    Clock.BronsteinConfig(60, 2).show must_== "1 d+2"
    Clock.BronsteinConfig(150, 10).show must_== "2.5 d+10"
    Clock.BronsteinConfig(0, 5).show must_== "0 d+5"
  }

  "Bronstiin showBerserk" in {
    Clock.BronsteinConfig(60, 2).showBerserk must_== "1 d+0"
    Clock.BronsteinConfig(150, 10).showBerserk must_== "2.5 d+0"
    Clock.BronsteinConfig(0, 5).showBerserk must_== Clock.BronsteinConfig(0, 5).show
  }

  "Simple Delay show" in {
    Clock.SimpleDelayConfig(60, 2).show must_== "1 d/2"
    Clock.SimpleDelayConfig(120, 12).show must_== "2 d/12"
    Clock.SimpleDelayConfig(240, 4).show must_== "4 d/4"
    Clock.SimpleDelayConfig(30, 4).show must_== "½ d/4"
    Clock.SimpleDelayConfig(0, 10).show must_== "0 d/10"
  }

  "Simple Delay showBerserk" in {
    Clock.SimpleDelayConfig(60, 2).showBerserk must_== "½ d/1"
    Clock.SimpleDelayConfig(120, 12).showBerserk must_== "1 d/6"
    Clock.SimpleDelayConfig(240, 4).showBerserk must_== "2 d/2"
    Clock.SimpleDelayConfig(30, 4).showBerserk must_== "¼ d/2"
    Clock.SimpleDelayConfig(0, 10).showBerserk must_== Clock.SimpleDelayConfig(0, 10).show
  }

  "Byoyomi show" in {
    ByoyomiClock.Config(600, 0, 20, 1).show must_== "10|20"
    ByoyomiClock.Config(300, 0, 20, 2).show must_== "5|20(2x)"
    ByoyomiClock.Config(300, 10, 20, 2).show must_== "5+10|20(2x)"
    ByoyomiClock.Config(60, 10, 0, 1).show must_== "1+10"
    ByoyomiClock.Config(0, 10, 0, 1).show must_== "0+10"
    ByoyomiClock.Config(0, 0, 10, 1).show must_== "0|10"
    ByoyomiClock.Config(600, 0, 0, 1).show must_== "10|0"
  }

  "Byoyomi showBerserk" in {
    ByoyomiClock.Config(600, 0, 20, 1).showBerserk must_== "5|10"
    ByoyomiClock.Config(300, 0, 20, 2).showBerserk must_== "2.5|10(2x)"
    ByoyomiClock.Config(300, 10, 20, 2).showBerserk must_== "5|10(2x)"
    ByoyomiClock.Config(60, 10, 0, 1).showBerserk must_== "1|0"
    ByoyomiClock.Config(0, 10, 0, 1).showBerserk must_== ByoyomiClock.Config(0, 10, 0, 1).show
    ByoyomiClock.Config(0, 0, 10, 1).showBerserk must_== ByoyomiClock.Config(0, 0, 10, 1).show
    ByoyomiClock.Config(600, 0, 0, 1).showBerserk must_== "5|0"
  }
}
