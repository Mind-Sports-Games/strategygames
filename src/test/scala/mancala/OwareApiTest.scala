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

}
