package strategygames.fairysf

import org.playstrategy.FairyStockfish

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class FairyStockfishTest extends Specification with ValidatedMatchers {

  FairyStockfish.init()
  val emptyMoves = new FairyStockfish.VectorOfStrings()

  "Shogi initial fen" should {
    "be expected string" in {
      format.FEN(
        "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL[] w - - 0 1"
      ) === variant.Shogi.initialFen
    }
  }

  "Shogi initial fen" should {
    "be valid" in {
      FairyStockfish.validateFEN(
        variant.Shogi.fishnetKey,
        variant.Shogi.initialFen.value
      ) === true
    }
  }

  "Shogi initial fen minus middle rank" should {
    "be invalid" in {
      FairyStockfish.validateFEN(
        variant.Shogi.fishnetKey,
        "lnsgkgsnl/1r5b1/ppppppppp/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL[] w - - 0 1"
      ) === false
    }
  }

  "Random string" should {
    "be invalid fen" in {
      FairyStockfish.validateFEN(
        variant.Shogi.fishnetKey,
        "I'm a Shogi FEN! (not)"
      ) === false
    }
  }

  "Amazons initial fen" should {
    "be expected string" in {
      format.FEN(
        "3q2q3/10/10/q8q/10/10/Q8Q/10/10/3Q2Q3[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppppppppppppppppp] w - - 0 1"
      ) === variant.Amazons.initialFen
    }
  }

}
