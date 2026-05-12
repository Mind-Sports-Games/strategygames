package strategygames.format

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class LexicalUciTest extends Specification with ValidatedMatchers {

  "d8d10" should {
    "be a valid Uci" in {
      LexicalUci.validUci("d8d10") === true
    }
  }

  "d8d7,d7d9" should {
    "be a valid Uci" in {
      LexicalUci.validUci("d8d7,d7d9") === true
    }
  }
  "f9e10,e10f10" should {
    "be a valid Uci" in {
      LexicalUci.validUci("f9e10,e10f10") === true
    }
  }

  "d8d7,d7d9,d7d5" should {
    "not be a valid Uci" in {
      LexicalUci.validUci("d8d7,d7d9,d7d5") === false
    }
  }
}
