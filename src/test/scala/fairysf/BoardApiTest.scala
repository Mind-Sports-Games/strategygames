package strategygames.fairysf

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class FairyStockfishBoardApiTest extends Specification with ValidatedMatchers {

  "Shogi initial fen" should {
    val position = Api.positionFromVariant(variant.Shogi)
    "be valid" in {
      Api.validateFEN(
        variant.Shogi.fairysfName.name,
        variant.Shogi.initialFen.value
      ) must_== true
    }
    "return confirm sufficient material for both sides" in {
      position.insufficientMaterial must_== ((false, false))
    }
    "have legal moves" in {
      position.legalMoves.size must_!= 0L
    }
    "have 40 pieces" in {
      position.pieceMap.size must_== 40
    }
    "not be game end" in {
      position.gameEnd() must_== false
    }
    "not be optional game end" in {
      position.optionalGameEnd must_== false
    }
  }

  "Xiangqi initial fen" should {
    val position = Api.positionFromVariant(variant.Xiangqi)
    "equal expected initialfen" in {
      variant.Xiangqi.initialFen.value must_== "rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C5C1/9/RNBAKABNR w - - 0 1"
    }
    "be valid" in {
      Api.validateFEN(
        variant.Xiangqi.fairysfName.name,
        variant.Xiangqi.initialFen.value
      ) must_== true
    }
    "return confirm sufficient material for both sides" in {
      position.insufficientMaterial must_== ((false, false))
    }
    "have legal moves" in {
      position.legalMoves.size must_!= 0L
    }
    "have 32 pieces" in {
      position.pieceMap.size must_== 32
    }
    "have 32 pieces again" in {
      position.pieceMap.size must_== 32
    }
    "not be game end" in {
      position.gameEnd() must_== false
    }
    "not be optional game end" in {
      position.optionalGameEnd must_== false
    }
    "e1e2 opening provide valid piece map" in {
      val newPosition = position.makeMoves(List("e1e2"))
      newPosition.pieceMap.size must_== 32
    }
  }



  "Shogi four fold repetition" should {
    val position = Api.positionFromVariant(variant.Shogi)
    "be optional game end" in {
      val newPosition = position.makeMoves(
        List("h2i2", "b8a8", "i2h2", "a8b8", "h2i2", "b8a8", "i2h2", "a8b8", "h2i2", "b8a8", "i2h2", "a8b8")
      )
      newPosition.optionalGameEnd must_== true
    }
    "not be game end" in {
      val newPosition = position.makeMoves(
        List("h2i2", "b8a8", "i2h2", "a8b8", "h2i2", "b8a8", "i2h2", "a8b8", "h2i2", "b8a8", "i2h2", "a8b8")
      )
      newPosition.gameEnd() must_== false
    }
  }

  "Shogi Checkmate FEN" should {
    //https://www.pychess.org/cdRztJdY?ply=74
    val checkmateFen = "l2g1g1nl/5sk2/3p1p1p1/p3p1p1p/1n2n4/P4PP1P/1P1sPK1P1/5sR1+r/L4+p1N1[GPSBBglpp] w - - 4 38"
    val position = Api.positionFromVariantKeyAndFEN(variant.Shogi.key, checkmateFen)
    "be game end" in {
      position.gameEnd() must_== true
    }
    "be checkmate" in {
      position.gameResult must_== GameResult.Checkmate()
    }
    "have no legal moves" in {
      position.legalMoves.size must_== 0L
    }
    "have pieces in hand" in {
      position.piecesInHand.size must_== 9L
    }
  }

  "Shogi fools mate moves" should {
    val foolsFEN = "lnsg1gsnl/5rkb1/ppppppp+Pp/9/9/9/PPPPPPP1P/1B5R1/LNSGKGSNL[P] b - - 0 4"
    val positionStart = Api.positionFromVariant(variant.Shogi)
    val position = Api.positionFromVariantKeyAndFEN(variant.Shogi.key, foolsFEN)
    "produce fools mate fen" in {
      val newPosition = positionStart.makeMoves(
        List("h3h4", "e9f8", "h4h5", "f8g8", "h5h6", "b8f8", "h6h7+")
      )
      newPosition.fen.value must_== foolsFEN
    }
    "be game end" in {
      position.gameEnd() must_== true
    }
    "be checkmate" in {
      position.gameResult must_== GameResult.Checkmate()
    }
    "have no legal moves" in {
      position.legalMoves.size must_== 0L
    }
  }

  "Shogi stalemate is win" should {
    val stalemateFEN = "8l/8k/9/8P/9/2P6/PP1PPPP2/1B5R1/LNSGKGSNL[] b - - 0 2"
    val position = Api.positionFromVariantKeyAndFEN(variant.Shogi.key, stalemateFEN)
    "have no legal moves" in {
      position.legalMoves.size must_== 0L
    }
    "be game end" in {
      position.gameEnd() must_== true
    }
    "be checkmate" in {
      position.gameResult must_== GameResult.Checkmate()
    }
  }

  "Shogi king only" should {
    val insufficientMaterialFEN = "8k/9/9/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL[LNSGGSNLBRPPPPPPPPP] b - - 0 2"
    val position = Api.positionFromVariantKeyAndFEN(variant.Shogi.key, insufficientMaterialFEN)
    "never have insufficient material" in {
      position.insufficientMaterial must_== ((false, false))
    }
  }

  //https://www.pychess.org/ozI3pCRx
  "Shogi perpetual check" should {
    val position = Api.positionFromVariant(variant.Shogi)
    val position2 = position.makeMoves(
        List("c3c4", "e7e6", "b2g7+", "e9d8", "g7f6", "d8e9", "f6g7", "e9d8", "g7f6", "d8e9", "f6g7", "e9d8", "g7f6", "d8e9", "f6g7")
    )
    "should produce non optional game end" in {
      position2.gameEnd() must_== true
    }
  }
}
