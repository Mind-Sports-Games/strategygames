package strategygames.mancala
import strategygames.Player

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class OwareFenTest extends Specification with ValidatedMatchers {

    "OwarestoneArray from initial fen" should {
        val fen = variant.Oware.initialFen
        "be valid" in {
        fen.owareStoneArray must_== Array(4,4,4,4,4,4,4,4,4,4,4,4)
        }
    }

    "Initial fen scores and starting player" should {
        val fen = variant.Oware.initialFen
        "player 1 score be 0" in {
        fen.player1Score must_== 0
        }
        "player 2 score be 0" in {
        fen.player2Score must_== 0
        }
        "Starting player is South" in {
        fen.player must_== Some(Player.P1)
        }
    }

    "fen 1s,1s,4s,4s,4s,4s/4s,4s,1s,1s,4s,4s 4 8 N" should {
        val fen = strategygames.mancala.format.FEN("1s,1s,4s,4s,4s,4s/4s,4s,1s,1s,4s,4s 4 8 N")
        "player 1 score be 4" in {
        fen.player1Score must_== 4
        }
        "player 2 score be 8" in {
        fen.player2Score must_== 8
        }
        "Player turn is North" in {
        fen.player must_== Some(Player.P2)
        }
        "OwarestoneArray" in {
        fen.owareStoneArray must_== Array(4,4,1,1,4,4,4,4,4,4,1,1)
        }
    }

    "fen 5,13s/4s,1,1s,2,3s a B S" should {
        val fen = strategygames.mancala.format.FEN("5,13s/4s,1,1s,2,3s 27 2 S")
        "player 1 score be 4" in {
        fen.player1Score must_== 27
        }
        "player 2 score be 8" in {
        fen.player2Score must_== 2
        }
        "Player turn is North" in {
        fen.player must_== Some(Player.P1)
        }
        "OwarestoneArray" in {
        fen.owareStoneArray must_== Array(4,0,1,0,0,3,13,0,0,0,0,0)
        }
    }
    
}
