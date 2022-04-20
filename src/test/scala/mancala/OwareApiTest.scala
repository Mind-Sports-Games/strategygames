package strategygames.mancala

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class OwareApiTest extends Specification with ValidatedMatchers {

    "Oware initial fen" should {
        val fen = variant.Oware.initialFen.value
        "be valid" in {
            Api.validateFEN( fen ) must_== true
        }
    }

    "Oware invalid fen" should {
        val fen = "RRRRRR/RRRRRR E J S"
        "false due to too many pieces" in {
            Api.validateFEN(fen) must_== false
        }
        val fen2 = "3EEE/4RR 0 F N"
         "false due to only allowing 1's for empty spaces" in {
            Api.validateFEN(fen2) must_== false
        }
    }

    "Oware situation legal moves" should {
        val game = Api.position
        val newGame = game.makeMoves( List(0,8,2) )
        "5 legal moves" in {
            newGame.legalMoves.size must_== 5
        }
    }

    "Oware Move to Uci " should {
        val moves = List[Int](0,1,2,3,4,5,6,7,8,9,10,11)
        "possible moves convert to uci (0->a1 etc)" in {
            moves.map(m => Api.moveToUci(m)) must_== List("a1", "b1", "c1", "d1", "e1", "f1", "f2", "e2", "d2", "c2", "b2", "a2")
        }
    }
    
    "Oware Uci to Move " should {
        val uci = List[String]("a1", "b1", "c1", "d1", "e1", "f1", "a2", "b2", "c2", "d2", "e2", "f2")
        "possible moves convert from uci (a1->0 etc)" in {
            uci.map(m => Api.uciToMove(m)) must_== List[Int](0,1,2,3,4,5,11,10,9,8,7,6)
        }
    }

    "Piece map of Oware setup" should {
        val game = Api.position
        val pieceMap: PieceMap = game.pieceMap
        val roleAtA1: Role = pieceMap.get(Pos.A1) match {
            case Some(piece) => piece.role
            case None => OneStone
        }
        "12 starting pieces " in {
            pieceMap.size must_== 12
        }
        "fourstone in pos 0" in {
            roleAtA1 must_== FourStone
        }
    }

    "convertPieceMapFromFen" should {
        val game = Api.position
        val newGame = game.makeMoves(List(0,6,3,7,4))

        //val fen = "EGGHAB/AFE11F 0 0 N"
        val pieceMap = newGame.pieceMap
        val roleAtB1: Role = pieceMap.get(Pos.B1) match {
            case Some(piece) => piece.role
            case None => OneStone
        }
        "Six Stone at pos B1" in {
            roleAtB1 must_== SixStone
        }
         "10 pieces can move" in {
            pieceMap.size must_== 10
        }
    }

    "Piece map of Oware game" should {
        val game = Api.position
        val newGame = game.makeMoves(List(0,6,3,7,4))
        val position = Api.positionFromFen(newGame.fen.value)
        val fen = newGame.fen // looking for 577812 (top) 165--6 (bot)
        val pieceMap: PieceMap = position.pieceMap
        val roleAtC1: Role = pieceMap.get(Pos.C1) match {
            case Some(piece) => piece.role
            case None => OneStone
        }
        "fen after a few moves" in {
            fen.value must_== "EGGHAB/AFE11F 0 0 N"
        }
        "10 current pieces " in {
            pieceMap.size must_== 10
        }
        "fiveStone in pos 2/c1" in {
            roleAtC1 must_== FiveStone
        }
    }

    "owareBoardFromFen" should {
        val game = Api.position
        val newGame = game.makeMoves(List(0,7,4))
        val fen = newGame.fen
        val owareBoard = Api.owareBoardFromFen(fen.value)
        "fen is EEFFAE/1EEE1E 0 0 N" in {
            fen.value must_== "EEFFAE/1EEE1E 0 0 N"
        }
        "oware board is " in {
            owareBoard.position must_== Array(0,5,5,5,0,5,5,1,6,6,5,5,0,0)
        }
    }

    "positionFromVariantAndMoves" should {
        val uciMoves = List("a1e1","e2a2","e1c2") // 0,7,4
        val pos = Api.positionFromVariantAndMoves(variant.Oware, uciMoves)
        val fen = pos.fen
        "fen is EEFFAE/1EEE1E 0 0 N" in {
            fen.value must_== "EEFFAE/1EEE1E 0 0 N"
        }
        "legal moves are 6,7,8,9,10,11" in {
            pos.legalMoves must_== Array(7,11,10,9,8,6) // interesting order i guess we should not case about this.....?
        }
        "6 legal moves" in {
            pos.legalMoves.size must_== 6
        }

    }

    "positionFromVariantNameAndFEN" should {
        val fen = "B1111A/AA111A U U N"
        val pos = Api.positionFromVariantNameAndFEN("oware", fen)
        "fen from new pos" in {
            pos.fen.value must_== fen
        }
    }

    "pieceMapFromFen" should {
        val fen = "B1111A/AA111A U U N"
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
        val fen = "B1111A/AA111A U U N"
        val position = Api.positionFromFen(fen)
        "is still ongoing during game" in {
            position.gameResult must_== GameResult.Ongoing()
        }
    }

    "game result" should {
        val fen = "B1111A/AA111A Y Q N"
        val position = Api.positionFromFen(fen)
        "is variantEnd if player has score above 24" in {
            position.gameResult must_== GameResult.VariantEnd()
        }
    }

    "game result" should {
        val fen = "111111/111111 X X S"
        val position = Api.positionFromFen(fen)
        "be a draw with same score and no pieces" in {
            position.gameResult must_== GameResult.Draw()
        }
    }



}
