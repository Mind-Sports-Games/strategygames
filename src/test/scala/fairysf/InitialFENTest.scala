package strategygames.fairysf

import org.playstrategy.FairyStockfish

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class InitialFenTest extends Specification with ValidatedMatchers {
  "fairystockfish" should {
    "be inititalized" in {
      FairyStockfish.init()
      true must_== true
    }
  }

  "shogi" should {
    "have fen" in {
      format.FEN("rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C2C4/9/RNBAKABNR b - - 1 1") must_== variant.Shogi.initialFen
    }
  }

}
