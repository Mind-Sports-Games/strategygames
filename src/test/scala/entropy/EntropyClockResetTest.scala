package strategygames.entropy

import org.specs2.mutable.Specification

import strategygames.{ ByoyomiClock, Centis, Clock, Player }

class EntropyClockResetTest extends Specification {

  "resetting a fischer clock" should {
    val clock = Clock.apply(Clock.Config(600, 2))
    val used  = clock.giveTime(Player.P1, Centis(-30000)).giveTime(Player.P2, Centis(-12000))

    "restore both players to the full starting time" in {
      used.remainingTime(Player.P1) must not(be_==(clock.remainingTime(Player.P1)))

      val reset = used.resetToStart
      reset.remainingTime(Player.P1) === clock.remainingTime(Player.P1)
      reset.remainingTime(Player.P2) === clock.remainingTime(Player.P2)
    }

    "hand the clock back to P1, who is Chaos for the new round" in {
      used.resetToStart.player === Player.P1
    }

    "keep the config" in {
      used.resetToStart.config === clock.config
    }

    "carry berserk over, so it is not refunded by the swap" in {
      val berserked = clock.goBerserk(Player.P1)
      val reset     = berserked.resetToStart

      reset.berserked(Player.P1) must beTrue
      reset.berserked(Player.P2) must beFalse
      // a berserked player restarts on the reduced clock, not the full one
      reset.remainingTime(Player.P1) must be_<(reset.remainingTime(Player.P2))
    }
  }

  "resetting a byoyomi clock" should {
    val clock = ByoyomiClock.apply(ByoyomiClock.Config(600, 0, 30, 3))

    "restore the main time and the periods" in {
      val used  = clock.giveTime(Player.P1, Centis(-30000))
      val reset = used.resetToStart

      reset.remainingTime(Player.P1) === clock.remainingTime(Player.P1)
      reset.player === Player.P1
      reset.config === clock.config
    }

    "carry berserk over" in {
      clock.goBerserk(Player.P1).resetToStart.berserked(Player.P1) must beTrue
    }
  }
}
