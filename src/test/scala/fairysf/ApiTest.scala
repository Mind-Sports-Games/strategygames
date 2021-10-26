package strategygames.fairysf

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class FairyStockfishApiTest extends Specification with ValidatedMatchers {

  "Shogi initial fen" should {
    "be valid" in {
      Api.validateFEN(
        variant.Shogi.fairysfName.name,
        variant.Shogi.initialFen.value
      ) must_== true
    }
  }

  "Shogi initial fen minus middle rank" should {
    "be invalid" in {
      Api.validateFEN(
        variant.Shogi.fairysfName.name,
        "lnsgkgsnl/1r5b1/ppppppppp/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL[-] w 0 1"
      ) must_== false
    }
  }

  "Random string" should {
    "be invalid fen" in {
      Api.validateFEN(
        variant.Shogi.fairysfName.name,
        "I'm a Shogi FEN! (not)"
      ) must_== false
    }
  }

  "Chess black Checkmate FEN" should {
    "be game end" in {
      Api.gameResult(
        "chess",
        //https://www.pychess.org/cdRztJdY?ply=74
        "rnb1kbnr/pppp1ppp/8/4p3/5PPq/8/PPPPP2P/RNBQKBNR w KQkq - 1 3",
      ) must_== GameResult.Checkmate()
      Api.gameResult(
        "chess",
        //https://www.pychess.org/cdRztJdY?ply=74
        "r1bqkbnr/1ppppQ1p/p1n3p1/8/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4",
      ) must_== GameResult.Checkmate()
    }
  }
  "Shogi Checkmate FEN" should {
    "have no legal moves" in {
      Api.legalMoves(
        variant.Shogi.fairysfName.name,
        //https://www.pychess.org/cdRztJdY?ply=74
        "l2g1g1nl/5sk2/3p1p1p1/p3p1p1p/1n2n4/P4PP1P/1P1sPK1P1/5sR1+r/L4+p1N1[GPSBBglpp] w - - 4 38",
      ).size must_== 0L
    }
  }

  "availableVariants" should {
    "should have shogi" in {
      Api.availableVariants().filter(_=="shogi").size must_== 1
    }
    "should have xiangqi" in {
      Api.availableVariants().filter(_=="xiangqi").size must_== 1
    }
    "should not have my little pony" in {
      Api.availableVariants().filter(_=="my little pony").size must_== 0
    }
  }
}
