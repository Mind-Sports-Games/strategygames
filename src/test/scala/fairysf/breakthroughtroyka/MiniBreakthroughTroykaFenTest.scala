package strategygames.fairysf

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification
import strategygames.fairysf.variant.MiniBreakthroughTroyka

class MiniBreakthroughTroykaFenTest extends Specification with ValidatedMatchers {
  "MiniBreakthroughTroyka initial fen" should {
    val fen    = MiniBreakthroughTroyka.initialFen
    val pieces = fen.toString()

    "be valid" in {
      pieces must_== "ppppp/ppppp/5/PPPPP/PPPPP w - - 0 1"
    }
  }
}
