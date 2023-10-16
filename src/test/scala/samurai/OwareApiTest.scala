package strategygames.samurai

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class OwareApiTest extends Specification with ValidatedMatchers {

  "Oware initial fen" should {
    val fen = variant.Oware.initialFen.value
    "be valid" in {
      Api.validateFEN(fen) must_== true
    }
  }

  "Oware invalid fen" should {
    val fen  = "18S,18S,18S,18S,18S,18S/18S,18S,18S,18S,18S,18S 5 10 S 50"
    "false due to too many pieces" in {
      Api.validateFEN(fen) must_== false
    }
    val fen2 = "2S,4,1S/1S,1S,3,1S 21 21 N 50"
    "true due to allowing >1 for multiple empty spaces" in {
      Api.validateFEN(fen2) must_== true
    }
    val fen3 = "3,5S,5S,5S/4,18S,18S,2 0 6 N 50"
    "false due to more space than width" in {
      Api.validateFEN(fen3) must_== false
    }
  }

  "Oware situation legal moves" should {
    val game    = Api.position
    val newGame = game.makeMoves(List(0, 8, 2))
    "5 legal moves" in {
      newGame.legalMoves.size must_== 5
    }
  }

  "Oware Move to Uci " should {
    val moves = List[Int](0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
    "possible moves convert to uci (0->a1 etc)" in {
      moves.map(m => Api.moveToUci(m)) must_== List(
        "a1",
        "b1",
        "c1",
        "d1",
        "e1",
        "f1",
        "f2",
        "e2",
        "d2",
        "c2",
        "b2",
        "a2"
      )
    }
  }

  "Oware Uci to Move " should {
    val uci = List[String]("a1", "b1", "c1", "d1", "e1", "f1", "a2", "b2", "c2", "d2", "e2", "f2")
    "possible moves convert from uci (a1->0 etc)" in {
      uci.map(m => Api.uciToMove(m)) must_== List[Int](0, 1, 2, 3, 4, 5, 11, 10, 9, 8, 7, 6)
    }
  }

  "Piece map of Oware setup" should {
    val game               = Api.position
    val pieceMap: PieceMap = game.pieceMap
    val countAtA1: Int     = pieceMap.get(Pos.A1).map(_._2).getOrElse(0)
    "12 starting pieces " in {
      pieceMap.size must_== 12
    }
    "Four Stones in pos 0" in {
      countAtA1 must_== 4
    }
  }

  "convertPieceMapFromFen" should {
    val game    = Api.position
    val newGame = game.makeMoves(List(0, 6, 3, 7, 4))

    // val fen = "5S,7S,7S,8S,1S,2s/1S,6S,5S,2,6s 0 0 N 50"
    val pieceMap       = newGame.pieceMap
    val countAtB1: Int = pieceMap.get(Pos.B1).map(_._2).getOrElse(0)
    val countAtD1: Int = pieceMap.get(Pos.D1).map(_._2).getOrElse(0)
    val countAtE1: Int = pieceMap.get(Pos.E1).map(_._2).getOrElse(0)
    "Six Stones at pos B1" in {
      countAtB1 must_== 6
    }
    "Zero Stones at pos D1" in {
      countAtD1 must_== 0
    }
    "Zero Stones at pos E1" in {
      countAtE1 must_== 0
    }
    "10 pieces can move" in {
      pieceMap.size must_== 10
    }
  }

  "Piece map of Oware game" should {
    val game               = Api.position
    val newGame            = game.makeMoves(List(0, 6, 3, 7, 4))
    val position           = Api.positionFromFen(newGame.fen.value)
    val fen                = newGame.fen // looking for 577812 (top) 165--6 (bot)
    val pieceMap: PieceMap = position.pieceMap
    val countAtC1: Int     = pieceMap.get(Pos.C1).map(_._2).getOrElse(0)
    "fen after a few moves" in {
      fen.value must_== "5S,7S,7S,8S,1S,2S/1S,6S,5S,2,6S 0 0 N 3"
    }
    "10 current pieces " in {
      pieceMap.size must_== 10
    }
    "fiveStone in pos 2/c1" in {
      countAtC1 must_== 5
    }
  }

  "owareBoardFromFen" should {
    val game       = Api.position
    val newGame    = game.makeMoves(List(0, 7, 4))
    val fen        = newGame.fen
    val owareBoard = Api.owareBoardFromFen(fen.value)
    "fen is 5S,5S,6S,6S,1S,5S/1,5S,5S,5S,1,5S 0 0 N 3" in {
      fen.value must_== "5S,5S,6S,6S,1S,5S/1,5S,5S,5S,1,5S 0 0 N 2"
    }
    "oware board is " in {
      owareBoard.position must_== Array(0, 5, 5, 5, 0, 5, 5, 1, 6, 6, 5, 5, 0, 0)
    }
  }

  "positionFromVariantAndMoves" should {
    val uciMoves = List("a1e1", "e2a2", "e1c2") // 0,7,4
    val pos      = Api.positionFromVariantAndMoves(variant.Oware, uciMoves)
    val fen      = pos.fen
    "fen is 5S,5S,6S,6S,1S,5S/1,5S,5S,5S,1,5S 0 0 N 3" in {
      fen.value must_== "5S,5S,6S,6S,1S,5S/1,5S,5S,5S,1,5S 0 0 N 2"
    }
    "legal moves are 6,7,8,9,10,11" in {
      pos.legalMoves must_== Array(7, 11, 10, 9, 8,
        6) // interesting order i guess we should not case about this.....?
    }
    "6 legal moves" in {
      pos.legalMoves.size must_== 6
    }

  }

  "positionFromVariantNameAndFEN" should {
    val fen = "2S,4,1S/1S,1S,3,1S 21 21 N 50"
    val pos = Api.positionFromVariantNameAndFEN("oware", fen)
    "fen from new pos" in {
      pos.fen.value must_== fen
    }
  }

  "pieceMapFromFen" should {
    val fen      = "2S,4,1S/1S,1S,3,1S 21 21 N 50"
    val pieceMap = Api.pieceMapFromFen("oware", fen)
    "6 pieces" in {
      pieceMap.size must_== 5
    }
    "possible moves a1, b1, f1, a2, f2" in {
      pieceMap.keys.toList.contains(Pos.A1) must_== true
      pieceMap.keys.toList.contains(Pos.A2) must_== true
      pieceMap.keys.toList.contains(Pos.B1) must_== true
      pieceMap.keys.toList.contains(Pos.F1) must_== true
      pieceMap.keys.toList.contains(Pos.F2) must_== true
    }
  }

  "game result " should {
    val fen      = "2S,4,1S/1S,1S,3,1S 21 21 N 50"
    val position = Api.positionFromFen(fen)
    "is still ongoing during game" in {
      position.gameResult must_== GameResult.Ongoing()
    }
  }

  "game result" should {
    val fen      = "2S,4,1S/1S,1S,3,1S 25 17 N 50"
    val position = Api.positionFromFen(fen)
    "is variantEnd if player has score above 24" in {
      position.gameResult must_== GameResult.VariantEnd()
    }
  }

  "game result" should {
    val fen      = "6/6 24 24 S 50"
    val position = Api.positionFromFen(fen)
    "be a draw with same score and no pieces" in {
      position.gameResult must_== GameResult.Draw()
    }
  }

  "game result" should {
    val fen      = "1,1S,1S,1S,1S,2S/6 22 20 S 50"
    val position = Api.positionFromFen(fen)
    "end game when no legal moves" in {
      position.gameResult must_== GameResult.VariantEnd()
    }
    "player with more total stones wins" in {
      position.gameOutcome must_== -1000
    }
  }

  "game result" should {
    val fen      = "1,1S,1S,1S,1S,2S/6 20 22 S 50"
    val position = Api.positionFromFen(fen)
    "end game when no legal moves" in {
      position.gameResult must_== GameResult.VariantEnd()
    }
    "player with more total stones wins" in {
      position.gameOutcome must_== -1000
    }
  }

  "game result" should {
    val fen      = "4S,4S,4S,4S,4S,4S/4S,4S,4S,4S,4S,4S 0 0 S 1" // starting pos
    val position = Api.positionFromFen(fen)
    val moves    = List(3, 9, 5, 7, 4, 10, 1, 8, 2, 7, 0, 6, 5, 6, 0, 10, 0, 11, 0, 9, 0, 7, 5, 8, 1, 9, 2, 11,
      3, 6, 0, 7, 1, 10, 4, 7, 3, 6, 1, 11, 2, 8, 4, 7, 5, 7, 3, 8, 4, 9, 5)
    position.makeMoves(moves)
    "end in draw after given moves" in {
      position.gameResult must_== GameResult.Draw()
    }
  }

  "game result" should {
    val fen      = "4S,4S,4S,4S,4S,4S/4S,4S,4S,4S,4S,4S 0 0 S 1" // starting pos
    val position = Api.positionFromFen(fen)
    val moves    = List(3, 9, 5, 7, 4, 10, 1, 8, 2, 7, 0, 6, 5, 6, 0, 10, 0, 11, 0, 9, 0, 7, 5, 8, 1, 9, 2, 11,
      3, 6, 0, 7, 1, 10, 4, 7, 3, 6, 1, 11, 2, 8, 4, 7, 5, 7, 3, 8, 4, 9, 5)
    position.makeMoves(moves)
    "end in draw after given moves" in {
      position.gameResult must_== GameResult.Draw()
    }
  }

  // https://playstrategy.org/Pd83xW5h/p2
  "game result" should {
    val fen      = "4S,4S,4S,4S,4S,4S/4S,4S,4S,4S,4S,4S 0 0 S 1" // starting pos
    val position = Api.positionFromFen(fen)
    val moves    = List(3, 7, 5, 10, 2, 9, 2, 10, 5, 8, 1, 8, 3, 7, 0, 10, 3, 9, 4, 8, 0, 11, 4, 10, 0, 9, 5, 7,
      0, 10, 1, 9, 4, 7, 3, 11, 5, 6, 0, 8, 0, 11, 0, 10, 2, 8, 1, 7, 3, 9, 2, 10, 3, 8, 5, 8, 1, 6, 2, 7, 4,
      6, 3, 10, 4, 7, 5, 9, 1, 10, 0, 8, 2, 7, 1, 6, 3, 8, 2, 7, 4, 9, 3, 8, 4, 9, 5, 10, 1, 7, 0, 6, 2, 8, 1,
      7, 3, 9, 2, 8, 4, 10, 3, 9, 4, 10, 5, 11, 5, 10, 4, 9, 3, 8, 4, 7, 2, 10, 3, 9, 4, 8, 5, 11, 1, 10, 0,
      7, 2, 6, 4, 9, 3, 10, 1, 8, 4, 7, 2, 10, 3, 9)
    position.makeMoves(moves)
    "end in draw after cycle spilts stones" in {
      position.gameResult must_== GameResult.Draw()
    }
  }

}
