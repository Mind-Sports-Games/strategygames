package strategygames.entropy

import org.specs2.mutable.Specification

import strategygames.{ ByoyomiClock, Centis, Clock, Player }

class EntropyClockResetTest extends Specification {

  "resetting a fischer clock" should {
    val clock = Clock.apply(Clock.Config(600, 2))
    val used  = clock.giveTime(Player.P1, Centis(-30000)).giveTime(Player.P2, Centis(-12000))

    "restore both players to the full starting time" in {
      used.remainingTime(Player.P1) must not(be_==(clock.remainingTime(Player.P1)))

      val reset = used.resetToStart(Player.P2)
      reset.remainingTime(Player.P1) === clock.remainingTime(Player.P1)
      reset.remainingTime(Player.P2) === clock.remainingTime(Player.P2)
    }

    "hand the clock to the player to move, who is Chaos for the new round" in {
      used.resetToStart(Player.P2).player === Player.P2
    }

    "keep the config" in {
      used.resetToStart(Player.P2).config === clock.config
    }

    "carry berserk over, so it is not refunded by the swap" in {
      val berserked = clock.goBerserk(Player.P1)
      val reset     = berserked.resetToStart(Player.P2)

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
      val reset = used.resetToStart(Player.P1)

      reset.remainingTime(Player.P1) === clock.remainingTime(Player.P1)
      reset.player === Player.P1
      reset.config === clock.config
    }

    "carry berserk over" in {
      clock.goBerserk(Player.P1).resetToStart(Player.P2).berserked(Player.P1) must beTrue
    }
  }

  "a game of entropy with a clock" should {
    "restart both clocks for the new round's Chaos when the roles swap" in {
      val clock   = Clock.apply(Clock.Config(600, 0))
      val start   = Game.apply(variant.Entropy).copy(clock = Some(clock))
      def forced(g: Game): Game =
        g.situation.flaggedAction.fold(g) {
          case m: Move         => g.apply(m)
          case d: Drop         => g.apply(d)
          case p: Pass         => g.apply(p)
          case dc: DrawCounter => g.apply(dc)
        }
      val games   = Iterator.iterate(start)(forced).take(400).toList
      val swapped = games.find(_.situation.board.round == 2)

      swapped must beSome[Game].like { case g =>
        g.clock.map(_.player) === Some(Player.P2)
        // P2's clock is already running again, so allow for the moment it has been ticking
        g.clock.map(_.remainingTime(Player.P1)) === Some(clock.remainingTime(Player.P1))
        g.clock.exists(_.remainingTime(Player.P2) >= Centis(59000)) must beTrue
      }
    }
  }
}
