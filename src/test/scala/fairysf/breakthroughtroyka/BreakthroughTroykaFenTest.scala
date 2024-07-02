package strategygames.fairysf

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification
import strategygames.fairysf.variant.MiniBreakthroughTroyka
import strategygames.fairysf.variant.BreakthroughTroyka
import strategygames.Player

class BreakthroughTroykaFenTest extends Specification with ValidatedMatchers {
  "BreakthroughTroyka initial fen" should {
    val fen    = BreakthroughTroyka.initialFen
    val pieces = fen.toString()

    "be valid" in {
      pieces must_== "pppppppp/pppppppp/8/8/8/8/PPPPPPPP/PPPPPPPP w - - 0 1"
    }
    // @TODO: fairysf.format.Forysth.<<@ which converts a FEN into a Situation.
    // And then tests initialfen and a custom position

    "Starting player is white/p1" in {
      fen.player must_== Some(Player.P1)
    }
  }

  "MiniBreakthroughTroyka initial fen" should {
    val fen    = MiniBreakthroughTroyka.initialFen
    val pieces = fen.toString()

    "be valid" in {
      pieces must_== "ppppp/ppppp/5/PPPPP/PPPPP w - - 0 1"
    }

    "Starting player is white/p1" in {
      fen.player must_== Some(Player.P1)
    }
  }
}
