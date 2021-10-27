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
      ) must_== ((false, false))
    }
    "have legal moves" in {
      Api.legalMoves(
        variant.Shogi.fairysfName.name,
        variant.Shogi.initialFen.value
      ).size must_!= 0L
    }
    "not be game end" in {
      Api.gameEnd(
        variant.Shogi.fairysfName.name,
        variant.Shogi.initialFen.value
      ) must_== false
    }
    "not be optional game end" in {
      Api.optionalGameEnd(
        variant.Shogi.fairysfName.name,
        variant.Shogi.initialFen.value
      ) must_== false
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
      Api.gameEnd(
        "chess",
        "rnb1kbnr/pppp1ppp/8/4p3/5PPq/8/PPPPP2P/RNBQKBNR w KQkq - 1 3"
      ) must_== true
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

  "Chess three fold repetition" should {
    "be optional game end" in {
      Api.optionalGameEnd(
        "chess",
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
        Some(List("b1c3", "g8f6", "c3b1", "f6g8", "b1c3", "g8f6", "c3b1", "f6g8"))
      ) must_== true
    }
    "not be game end" in {
      Api.gameEnd(
        "chess",
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
        Some(List("b1c3", "g8f6", "c3b1", "f6g8", "b1c3", "g8f6", "c3b1", "f6g8"))
      ) must_== false
    }
  }

  "Shogi four fold repetition" should {
    "be optional game end" in {
      Api.optionalGameEnd(
        variant.Shogi.fairysfName.name,
        variant.Shogi.initialFen.value,
        Some(List("h2i2", "b8a8", "i2h2", "a8b8", "h2i2", "b8a8", "i2h2", "a8b8", "h2i2", "b8a8", "i2h2", "a8b8"))
      ) must_== true
    }
    "not be game end" in {
      Api.gameEnd(
        variant.Shogi.fairysfName.name,
        variant.Shogi.initialFen.value,
        Some(List("h2i2", "b8a8", "i2h2", "a8b8", "h2i2", "b8a8", "i2h2", "a8b8", "h2i2", "b8a8", "i2h2", "a8b8"))
      ) must_== false
    }
  }

  "Shogi Checkmate FEN" should {
    //https://www.pychess.org/cdRztJdY?ply=74
    val checkmateFen = "l2g1g1nl/5sk2/3p1p1p1/p3p1p1p/1n2n4/P4PP1P/1P1sPK1P1/5sR1+r/L4+p1N1[GPSBBglpp] w - - 4 38"
    "be game end" in {
      Api.gameEnd(
        variant.Shogi.fairysfName.name,
        checkmateFen
      ) must_== true
    }
    "be checkmate" in {
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
    "be game end" in {
      Api.gameEnd(
        variant.Shogi.fairysfName.name,
        foolsFEN
      ) must_== true
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

  "Chess stalemate is draw" should {
    val stalemateFEN = "5bnr/4p1pq/4Qpkr/7p/7P/4P3/PPPP1PP1/RNB1KBNR b KQ - 2 10"
    "moves produce stalemate fen" in {
      Api.fenFromMoves(
        "chess",
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
        Some(List("e2e3", "a7a5", "d1h5", "a8a6", "h5a5", "h7h5", "a5c7", "a6h6", "h2h4", "f7f6", "c7d7", "e8f7", "d7b7", "d8d3", "b7b8", "d3h7", "b8c8", "f7g6", "c8e6"))
      ).value must_== stalemateFEN
    }
    "have no legal moves" in {
      Api.legalMoves(
        "chess",
        stalemateFEN,
      ).size must_== 0L
    }
    "be game end" in {
      Api.gameEnd(
        "chess",
        stalemateFEN
      ) must_== true
    }
    "be stalemate" in {
      Api.gameResult(
        "chess",
        stalemateFEN,
      ) must_== GameResult.Draw()
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
    "be game end" in {
      Api.gameEnd(
        variant.Shogi.fairysfName.name,
        stalemateFEN
      ) must_== true
    }
    "be checkmate" in {
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
      ) must_== ((false, true))
    }
  }

  "Shogi king only" should {
    val insufficientMaterialFEN = "8k/9/9/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL[LNSGGSNLBRPPPPPPPPP] b - - 0 2"
    "never have insufficient material" in {
      Api.insufficientMaterial(
        variant.Shogi.fairysfName.name,
        insufficientMaterialFEN
      ) must_== ((false, false))
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
