package strategygames.mancala

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import strategygames.{ Player }

class OwareVariantTest extends Specification with ValidatedMatchers {

    "valid moves in situation" should {
        val board = Board.init(variant.Oware)
        val p1 = Player(true)
        val situation = Situation(board, p1)
        
        val moves = variant.Oware.validMoves(situation)
        //val m = situation.moves
        "be valid" in {
            moves.size must_== 6
        }
    }


    "valid moves in situation" should {
        val board = Board.init(variant.Oware)
        val p1 = Player(true)
        val situation = Situation(board, p1)
        val moves = variant.Oware.validMoves(situation)
        val uci = moves.get(Pos.C1) match {
            case Some(move) => move.head.toUci.uci
            case None => ""
        }
        "c1 goes to f2" in {
            uci must_== "c1f2"
        }
    }

    "pos in second rank" should {
        val pos = Pos(6)
        val expected = Pos.at(5,1)
        "be backwards, Pos(6) is f2" in {
            pos must_== expected
        }
    }

    "grandslam in oware" should {
        val fen = "111BAB/A111BC T Q S"
        val position = Api.positionFromFen(fen)
        val newGame = position.makeTestMoves(List(5))
        "have an initial valid fen" in {
            Api.validateFEN(fen) must_== true
        }
        "is allowed the grandslam move" in {
            position.legalMoves.contains(5) must_== true
        }
        "current pos is ongoing" in {
            position.gameEnd must_== false
        }
        "but does not capture pieces once moved" in {
            newGame.gameEnd must_== false
        }
        "hence new new fen is valid" in {
            Api.validateFEN(newGame.fen.value) must_== true
        }
        "and equal to 111CBC/A111B1 T Q N" in {
            newGame.fen.value must_== "111CBC/A111B1 T Q N"
        }
    }

    "grandslam in oware" should {
        val fen = "A1H11C/BAABBB M M N"
        val position = Api.positionFromFen(fen)
        val newGame = position.makeTestMoves(List(9))
        "have an initial valid fen" in {
            Api.validateFEN(fen) must_== true
        }
        "is allowed the grandslam move" in {
            position.legalMoves.contains(9) must_== true
        }
        "current pos is ongoing" in {
            position.gameEnd must_== false
        }
        "but does not capture pieces once moved" in {
            newGame.gameEnd must_== false
        }
        "hence new new fen is valid" in {
            Api.validateFEN(newGame.fen.value) must_== true
        }
        "and equal to BA111C/CBBCCC M M S" in {
            newGame.fen.value must_== "BA111C/CBBCCC M M S"
        }
    }

    "grandslam in oware" should {
        val fen = "11111A/A1111A V W S"
        val position = Api.positionFromFen(fen)
        val newGame = position.makeTestMoves(List(5))
        "have an initial valid fen" in {
            Api.validateFEN(fen) must_== true
        }
        "is allowed the grandslam move" in {
            position.legalMoves.contains(5) must_== true
        }
        "current pos is ongoing" in {
            position.gameEnd must_== false
        }
        "but does not capture pieces once moved" in {
            newGame.gameEnd must_== false
        }
        "hence new new fen is valid" in {
            Api.validateFEN(newGame.fen.value) must_== true
        }
        "and equal to 11111B/A11111 V W N" in {
            newGame.fen.value must_== "11111B/A11111 V W N"
        }
    }

    "cycle position in oware" should {
        val fen = "A11111/11111A W W S"
        val position = Api.positionFromFen(fen)
        val newGame = position.makeTestMoves(List(5,11,0,6,1,7,2,8,3,9,4,10))
        "have an initial valid fen" in {
            Api.validateFEN(fen) must_== true
        }
        "have legal moves..." in {
            position.legalMoves.contains(5) must_== true
        }
        "currently be ongoing game" in {
            position.gameResult must_== GameResult.Ongoing()
        }
        "after cycle moves fens are the same" in {
            newGame.fen.value must_== fen
        }
        "and result in a draw" in {
            newGame.gameResult must_== GameResult.Draw()
        }
    }

    "not yet cycle position in oware" should {
        val fen = "A11111/11111A W W S"
        val position = Api.positionFromFen(fen)
        val newGame = position.makeTestMoves(List(5,11,0,6,1,7,2,8,3,9,4))
        "have an initial valid fen" in {
            Api.validateFEN(fen) must_== true
        }
        "have legal moves..." in {
            position.legalMoves.contains(5) must_== true
        }
        "currently be ongoing game" in {
            position.gameResult must_== GameResult.Ongoing()
        }
        "after 1 move from cycle moves fens are not the same" in {
            newGame.fen.value must_!= fen
        }
        "and not yet result in a draw" in {
            newGame.gameResult must_== GameResult.Ongoing()
        }
    }

    "cycle position in oware" should {
        val fen = "B11111/11111B U W S"
        val position = Api.positionFromFen(fen)
        val newGame = position.makeTestMoves(List(5,11,0,6,1,7,2,8,3,9,4,10))
        "have an initial valid fen" in {
            Api.validateFEN(fen) must_== true
        }
        "have legal moves..." in {
            position.legalMoves.contains(5) must_== true
        }
        "detected end game cycle" in {
            position.gameResult must_== GameResult.VariantEnd()
        }
        "and made North winner" in {
            position.gameOutcome must_== -1000
        }
        "api allows more moves - cycle moves fens are the same" in {
            newGame.fen.value must_== fen
        }
        "and result in also game end" in {
            newGame.gameResult must_== GameResult.VariantEnd()
        }
        "and still a win for North" in {
            newGame.gameOutcome must_== -1000
        }
    }

}
