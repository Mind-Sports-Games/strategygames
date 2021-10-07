package strategygames.fairysf

import FairyStockfish.init

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class FairySfTest extends Specification with ValidatedMatchers {

  "fairystockfish" should {
    "be inititalized" in {
      init()
      true must_== true
    }
  }

}
