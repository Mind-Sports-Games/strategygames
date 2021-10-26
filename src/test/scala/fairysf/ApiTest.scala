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
    "return confirm sufficient material for both sides" in {
      Api.insufficientMaterial(
        variant.Shogi.fairysfName.name,
        variant.Shogi.initialFen.value
      ) must_== (false, false)
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
        "rnb1kbnr/pppp1ppp/8/4p3/5PPq/8/PPPPP2P/RNBQKBNR w KQkq - 1 3",
      ) must_== GameResult.Checkmate()
      Api.gameResult(
        "chess",
        "r1bqkbnr/1ppppQ1p/p1n3p1/8/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4",
      ) must_== GameResult.Checkmate()
    }
  }

  "Shogi Checkmate FEN" should {
    //https://www.pychess.org/cdRztJdY?ply=74
    val checkmateFen = "l2g1g1nl/5sk2/3p1p1p1/p3p1p1p/1n2n4/P4PP1P/1P1sPK1P1/5sR1+r/L4+p1N1[GPSBBglpp] w - - 4 38"
    "be game end" in {
      Api.gameResult(
        variant.Shogi.fairysfName.name,
        checkmateFen
      ) must_== GameResult.Checkmate()
    }
    "have no legal moves" in {
      Api.legalMoves(
        variant.Shogi.fairysfName.name,
        checkmateFen
      ).size must_== 0L
    }
  }

  "Shogi fools mate moves" should {
    val foolsFEN = "lnsg1gsnl/5rkb1/ppppppp+Pp/9/9/9/PPPPPPP1P/1B5R1/LNSGKGSNL[P] b - - 0 4"
    "produce fools mate fen" in {
      Api.fenFromMoves(
        variant.Shogi.fairysfName.name,
        variant.Shogi.initialFen.value,
        Some(List("h3h4", "e9f8", "h4h5", "f8g8", "h5h6", "b8f8", "h6h7+"))
      ).value must_== foolsFEN
    }
    "be checkmate" in {
      Api.gameResult(
        variant.Shogi.fairysfName.name,
        foolsFEN,
      ) must_== GameResult.Checkmate()
    }
    "have no legal moves" in {
      Api.legalMoves(
        variant.Shogi.fairysfName.name,
        foolsFEN,
      ).size must_== 0L
    }
  }

  "Shogi stalemate is win" should {
    val stalemateFEN = "8l/8k/9/8P/9/2P6/PP1PPPP2/1B5R1/LNSGKGSNL[] b - - 0 2"
    "be a valid fen" in {
      Api.validateFEN(
        variant.Shogi.fairysfName.name,
        stalemateFEN
      ) must_== true
    }
    "have no legal moves" in {
      Api.legalMoves(
        variant.Shogi.fairysfName.name,
        stalemateFEN,
      ).size must_== 0L
    }
    "be stalemate" in {
      Api.gameResult(
        variant.Shogi.fairysfName.name,
        stalemateFEN,
      ) must_== GameResult.Checkmate()
    }
  }

  "Chess king only" should {
    val insufficientMaterialFEN = "4k3/8/8/8/8/8/PPPPPPPP/RNBQKBNR w KQ - 0 1"
    "have insufficient material" in {
      Api.insufficientMaterial(
        "chess",
        insufficientMaterialFEN
      ) must_== (false, true)
    }
  }

  "Shogi king only" should {
    val insufficientMaterialFEN = "8k/9/9/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL[LNSGGSNLBRPPPPPPPPP] b - - 0 2"
    "have insufficient material" in {
      Api.insufficientMaterial(
        variant.Shogi.fairysfName.name,
        insufficientMaterialFEN
      ) must_== (false, true)
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
