package strategygames.fairysf

import org.playstrategy.FairyStockfish

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class FairySFTestInit extends Specification with ValidatedMatchers {

  "fairystockfish" should {
    "be inititalized" in {
      FairyStockfish.init()
      true must_== true
    }
  }

}
