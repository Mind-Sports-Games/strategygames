package strategygames.abalone

import org.specs2.matcher.ValidatedMatchers

class AbaloneFenTest extends AbaloneTest with ValidatedMatchers {
    "initial default FEN (Belgian Daisy start position)" should {
        val fen    = variant.Abalone.initialFen
        val pieces = fen.pieces

        "have Black starting the game" in {
            fen.player must_== Some(P1)
            fen.value.split(' ').lift(3) must_== Some("b")
        }

        "have a total of 14 marbles per player" in {
            pieces.filter(p => p._2.player == P1).size must_== 14
            pieces.filter(p => p._2.player == P2).size must_== 14
        }

        "set the score to zero for each player" in {
            fen.player1Score must_== 0
            fen.player2Score must_== 0
        }

        "set both moves counters to zero" in {
            fen.fullMove.get must_== 0
            fen.halfMovesSinceLastCapture.get must_== 0
        }

        "draw a daisy of 7 marbles side by side on bottom for each player, then applies central symmetry on e5 to draw the ones on the top" in {
            pieces.get(Pos.A1) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.B1) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.D1) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.E1) must_== Some(Piece(P2, Stone))

            pieces.get(Pos.A2) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.B2) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.C2) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.D2) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.E2) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.F2) must_== Some(Piece(P2, Stone))

            pieces.get(Pos.B3) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.C3) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.E3) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.F3) must_== Some(Piece(P2, Stone))

            pieces.get(Pos.D7) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.E7) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.G7) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.H7) must_== Some(Piece(P1, Stone))

            pieces.get(Pos.D8) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.E8) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.F8) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.G8) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.H8) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.I8) must_== Some(Piece(P1, Stone))

            pieces.get(Pos.E9) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.F9) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.H9) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.I9) must_== Some(Piece(P1, Stone))
        }
    }

    "Snakes start position" should {
        val snakesFen = new format.FEN("sssss/s5/s6/s1SSSSS1/1s5S1/1sssss1S/6S/5S/SSSSS 0 0 b 0 0")
        val pieces = snakesFen.pieces

        "have Black starting the game" in {
            snakesFen.player must_== Some(P1)
            snakesFen.value.split(' ').lift(3) must_== Some("b")
        }

        "have a total of 14 marbles per player" in {
            pieces.filter(p => p._2.player == P1).size must_== 14
            pieces.filter(p => p._2.player == P2).size must_== 14
        }
    }

    "Atomouche start position" should {
        val atomoucheFen = new format.FEN("sS3/S2sSs/2s4/s1S4S/S2s1S2s/s4s1S/4S2/SsS2s/3sS 0 0 b 0 0")
        val pieces = atomoucheFen.pieces

        "have Black starting the game" in {
            atomoucheFen.player must_== Some(P1)
            atomoucheFen.value.split(' ').lift(3) must_== Some("b")
        }

        "have a total of 12 marbles per player" in {
            pieces.filter(p => p._2.player == P1).size must_== 12
            pieces.filter(p => p._2.player == P2).size must_== 12
        }

        "have each marble placed at the expected position" in {
            pieces.get(Pos.D1) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.B2) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.F2) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.A4) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.F4) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.D5) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.I5) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.B6) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.E7) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.G8) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.I8) must_== Some(Piece(P1, Stone))
            pieces.get(Pos.E9) must_== Some(Piece(P1, Stone))

            pieces.get(Pos.E1) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.A2) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.C2) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.E3) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.H4) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.A5) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.F5) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.D6) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.I6) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.D8) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.H8) must_== Some(Piece(P2, Stone))
            pieces.get(Pos.F9) must_== Some(Piece(P2, Stone))
        }
    }

    "Fun little game situation \"5/6/2s4/2ssss2/2SSsS3/1SSSsss1/2s4/1SSs2/3S1 5 3 b 11 42\"" should {
        val puzzleFen = new format.FEN("5/6/2s4/2ssss2/2SSsS3/1SSSsss1/2s4/1SSs2/3S1 5 3 b 11 42")
        val pieces = puzzleFen.pieces

        "have a score of 5 for P1 and a score of 3 for P2" in {
            puzzleFen.player1Score must_== 5
            puzzleFen.player2Score must_== 3
        }

        "have a total of 14 marbles per player, on the board and pushed out" in {
            pieces.filter(p => p._2.player == P1).size + puzzleFen.player2Score must_== 14
            pieces.filter(p => p._2.player == P2).size + puzzleFen.player1Score must_== 14
        }

        "11 plies were played since last time a marble was pushed out" in {
            puzzleFen.halfMovesSinceLastCapture must_== Some(11)
        }

        "42 moves were played in total" in {
            puzzleFen.fullMove must_== Some(42)
        }
    }

    // @TODO: keep adding interesting cases once we are able to instanciate a Board from a FEN.

    // "Game in progress since ages" should {
    //     val ongoingGame = new format.FEN("")

    //     "have a score of 3 for P2 and 2 for P1" in {
    //         fen.player1Score must_== 2
    //         fen.player2Score must_== 3
    //     }

    //     "be declared a draw" in {
    //       ...
    //      }
    // }

    /*
    "a score of 6" should {
        val winningByScoreFen = ""

        "have a winner" in {
        
        }
        "have no valid move" in {
        
        }
    }

    "a situation in which a player can not play" should {
        val stuckPlayerFen = new format.FEN("PPPPP/pppppP/5pP/6pP/7pP/7p/7/6/5 5 5 w 0 42")

        "have a winner" in {
            
        }
        "have no valid move" in {
        
        }
    }
    */
}