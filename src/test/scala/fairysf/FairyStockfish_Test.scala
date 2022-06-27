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
        "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL[-] w 0 1"
      ) must_== variant.Shogi.initialFen
    }
  }

  "Shogi initial fen" should {
    "be valid" in {
      FairyStockfish.validateFEN(
        variant.Shogi.fairysfName.name,
        variant.Shogi.initialFen.value
      ) must_== true
    }
  }

  "Shogi initial fen minus middle rank" should {
    "be invalid" in {
      FairyStockfish.validateFEN(
        variant.Shogi.fairysfName.name,
        "lnsgkgsnl/1r5b1/ppppppppp/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL[-] w 0 1"
      ) must_== false
    }
  }

  "Random string" should {
    "be invalid fen" in {
      FairyStockfish.validateFEN(
        variant.Shogi.fairysfName.name,
        "I'm a Shogi FEN! (not)"
      ) must_== false
    }
  }

  "Load flipersi" should {
    "result in this being a valid game" in {
      FairyStockfish.loadVariantConfig("""
[flipersi]
immobile = p
startFen = 8/8/8/8/8/8/8/8[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppp] w 0 1
pieceDrops = true
promotionPieceTypes = -
doubleStep = false
castling = false
stalemateValue = loss
stalematePieceCount = true
materialCounting = unweighted
enclosingDrop = reversi
enclosingDropStart = d4 e4 d5 e5
immobilityIllegal = false
flipEnclosedPieces = reversi
passOnStalemate = false

[flipello:flipersi]
startFen = 8/8/8/3pP3/3Pp3/8/8/8[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppp] w 0 1
passOnStalemate = true
        """)
      FairyStockfish.initialFen("flipello") must_== "8/8/8/3pP3/3Pp3/8/8/8[PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPpppppppppppppppppppppppppppppppp] w 0 1"
    }
  }
}
