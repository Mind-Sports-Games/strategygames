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

    "pos in second rank " should {
        val pos = Pos(6)
        val expected = Pos.at(5,1)
        "be backwards, Pos(6) is f2" in {
            pos must_== expected
        }
    }
}
