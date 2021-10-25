package strategygames.fairysf

import org.playstrategy.FairyStockfish

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class FairyStockfishTest extends Specification with ValidatedMatchers {

  val emptyMoves = new FairyStockfish.VectorOfStrings()

  "Shogi initial fen" should {
    "be expected string" in {
      FairyStockfish.init()
      format.FEN(
        "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL[-] w 0 1"
      ) must_== variant.Shogi.initialFen
    }
  }


  "Shogi initial fen" should {
    "be valid" in {
      FairyStockfish.init()
      FairyStockfish.validateFEN(
        variant.Shogi.fairysfName.name,
        variant.Shogi.initialFen.value
      ) must_== true
    }
  }

  "Shogi initial fen minus middle rank" should {
    "be invalid" in {
      FairyStockfish.init()
      FairyStockfish.validateFEN(
        variant.Shogi.fairysfName.name,
        "lnsgkgsnl/1r5b1/ppppppppp/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL[-] w 0 1"
      ) must_== false
    }
  }

  "Random string" should {
    "be invalid fen" in {
      FairyStockfish.init()
      FairyStockfish.validateFEN(
        variant.Shogi.fairysfName.name,
        "I'm a Shogi FEN! (not)"
      ) must_== false
    }
  }

  "Shogi initial FEN" should {
    "not be game end" in {
      FairyStockfish.init()
      FairyStockfish.isImmediateGameEnd(
        variant.Shogi.fairysfName.name,
        variant.Shogi.initialFen.value,
        emptyMoves
      ).get0() must_== false
    }
  }

  "Shogi Checkmate FEN" should {
    "be game end" in {
      FairyStockfish.init()
      FairyStockfish.isImmediateGameEnd(
        variant.Shogi.fairysfName.name,
        //https://www.pychess.org/cdRztJdY?ply=74
        "l2g1g1nl/5sk2/3p1p1p1/p3p1p1p/1n2n4/P4PP1P/1P1sPK1P1/5sR1+r/L4+p1N1[GPSBBglpp] w - - 4 38",
        emptyMoves
      ).get0() must_== true
    }
  }

}
