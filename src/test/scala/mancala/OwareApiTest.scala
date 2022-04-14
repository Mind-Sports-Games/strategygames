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
        "12 starting pieces " in {
            pieceMap.size must_== 12
        }
    }

}
