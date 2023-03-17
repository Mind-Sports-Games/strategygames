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
      position.gameEnd must_== false
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
      position.gameEnd must_== false
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
      newPosition.gameEnd must_== false
    }
  }

  "Shogi four fold repetition step by step" should {
    val position = Api.positionFromVariant(variant.Shogi)
    "be optional game end" in {
      val newPosition   = position.makeMoves(List("h2i2"))
      val newPosition2  = newPosition.makeMoves(List("b8a8"))
      val newPosition3  = newPosition2.makeMoves(List("i2h2"))
      val newPosition4  = newPosition3.makeMoves(List("a8b8"))
      val newPosition5  = newPosition4.makeMoves(List("h2i2"))
      val newPosition6  = newPosition5.makeMoves(List("b8a8"))
      val newPosition7  = newPosition6.makeMoves(List("i2h2"))
      val newPosition8  = newPosition7.makeMoves(List("a8b8"))
      val newPosition9  = newPosition8.makeMoves(List("h2i2"))
      val newPosition10 = newPosition9.makeMoves(List("b8a8"))
      val newPosition11 = newPosition10.makeMoves(List("i2h2"))
      val newPosition12 = newPosition11.makeMoves(List("a8b8"))
      newPosition12.optionalGameEnd must_== true
    }
  }

  "Shogi Checkmate FEN" should {
    // https://www.pychess.org/cdRztJdY?ply=74
    val checkmateFen =
      "l2g1g1nl/5sk2/3p1p1p1/p3p1p1p/1n2n4/P4PP1P/1P1sPK1P1/5sR1+r/L4+p1N1[GPSBBglpp] w - - 4 38"
    val position     = Api.positionFromVariantNameAndFEN(variant.Shogi.key, checkmateFen)
    "be game end" in {
      position.gameEnd must_== true
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
    val foolsFEN      = "lnsg1gsnl/5rkb1/ppppppp+Pp/9/9/9/PPPPPPP1P/1B5R1/LNSGKGSNL[P] b - - 0 4"
    val positionStart = Api.positionFromVariant(variant.Shogi)
    val position      = Api.positionFromVariantNameAndFEN(variant.Shogi.key, foolsFEN)
    "produce fools mate fen" in {
      val newPosition = positionStart.makeMoves(
        List("h3h4", "e9f8", "h4h5", "f8g8", "h5h6", "b8f8", "h6h7+")
      )
      newPosition.fen.value must_== foolsFEN
    }
    "be game end" in {
      position.gameEnd must_== true
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
    val position     = Api.positionFromVariantNameAndFEN(variant.Shogi.key, stalemateFEN)
    "have no legal moves" in {
      position.legalMoves.size must_== 0L
    }
    "be game end" in {
      position.gameEnd must_== true
    }
    "be stalemate" in {
      position.gameResult must_== GameResult.Stalemate()
    }
  }

  "Shogi king only" should {
    val insufficientMaterialFEN = "8k/9/9/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL[LNSGGSNLBRPPPPPPPPP] b - - 0 2"
    val position                = Api.positionFromVariantNameAndFEN(variant.Shogi.key, insufficientMaterialFEN)
    "never have insufficient material" in {
      position.insufficientMaterial must_== ((false, false))
    }
  }

  // https://www.pychess.org/ozI3pCRx
  "Shogi perpetual check" should {
    val position  = Api.positionFromVariant(variant.Shogi)
    val position2 = position.makeMoves(
      List(
        "c3c4",
        "e7e6",
        "b2g7+",
        "e9d8",
        "g7f6",
        "d8e9",
        "f6g7",
        "e9d8",
        "g7f6",
        "d8e9",
        "f6g7",
        "e9d8",
        "g7f6",
        "d8e9",
        "f6g7"
      )
    )
    "should produce non optional game end" in {
      position2.gameEnd must_== true
    }
  }

  // https://www.youtube.com/watch?v=6ehiWOSp_wk
  "Flipello quickest win" should {
    val position  = Api.positionFromVariant(variant.Flipello)
    val position2 = position.makeMoves(
      List("P@f4", "P@d3", "P@c4", "P@f5", "P@e6", "P@f3", "P@g4", "P@e3", "P@e2")
    )
    "should produce no legal moves" in {
      position2.legalMoves.size must_== 0L
    }
  }

  // https://www.vint.ee/en-gb/replay/12076188/
  // move 40 sets up 'passes' which Fairy-Stockfish calls 'd1d1'
  // Need to flip numbers from vint grid references to get equivalent Fairy-Stockfish uci
  "Flipello situation with passes" should {
    val position  = Api.positionFromVariant(variant.Flipello)
    val position2 = position.makeMoves(
      List(
        "P@d6",
        "P@c4",
        "P@f3",
        "P@f4",
        "P@e3",
        "P@e6",
        "P@c6",
        "P@f6",
        "P@c5",
        "P@c3",
        "P@d3",
        "P@f2",
        "P@f5",
        "P@d2",
        "P@b4",
        "P@a5",
        "P@b3",
        "P@d7",
        "P@a4",
        "P@a3",
        "P@c2",
        "P@b5",
        "P@e2",
        "P@d1",
        "P@g4",
        "P@h5",
        "P@h4",
        "P@h3",
        "P@e1",
        "P@f1",
        "P@g3",
        "P@h2",
        "P@b1",
        "P@b2",
        "P@a6",
        "P@a7",
        "P@b6",
        "P@b7",
        "P@c7",
        "P@g2",
        "P@a8",
        "P@c8",
        "P@a2"
      )
    )
    "should produce no drops in legal moves" in {
      position2.legalMoves.filterNot(_.startsWith("P@")).size must_!= 0L
    }
    val position3 = position2.makeMoves(List("d1d1"))
    "should produce legal moves after pass" in {
      position3.legalMoves.size must_!= 0L
    }
  }

  "Amazons initial position" should {
    val position = Api.positionFromVariant(variant.Amazons)
    // println("Possible moves in this position")
    // position.legalMoves.map(println(_))

    // why is fairy saying 146 moves?
    "should have 2176 legal moves" in {
      position.legalMoves.size must_== 2176L
    }
    "have 8 pieces" in {
      position.pieceMap.size must_== 8
    }
  }

  // TODO: was this ever passing?
  /*"Amazons requires full move " should {
    val position  = Api.positionFromVariant(variant.Amazons)
    val position2 = position.makeMoves(List("d1d6pd7"))

    "have 9 pieces (8 queens and pawn)" in {
      position2.pieceMap.size must_== 9
    }
  }*/
}
