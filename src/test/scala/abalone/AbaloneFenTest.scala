package strategygames.abalone

import org.specs2.matcher.ValidatedMatchers

import strategygames.{ Score, Status }

class AbaloneFenTest extends AbaloneTest with ValidatedMatchers {
  "initial default FEN (Belgian Daisy start position)" should {
    val fen       = variant.Abalone.initialFen
    val pieces    = fen.pieces
    val board     = Board(
      pieces,
      History(
        score = Score(0, 0)
      ),
      variant.Abalone
    )
    val situation = Situation(board, P1)

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

    "set both moves counters to expected initial value" in {
      fen.fullMove.get must_== 1
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

    // @TODO: ensure other types of moves are generated correctly when validMoves does work entirely
    "see 14 marbles able to move as 1" in {
      board.variant.validMovesOf1(situation).size must_== 14
    }
    // @TODO: ensure other types of moves are generated correctly when validMoves does work entirely
    "see 20 different moves of 1 marble" in {
      board.variant.validMovesOf1(situation).foldLeft(0)(_ + _._2.size) must_== 20
    }
  }

  "Snakes start position" should {
    val snakesFen = new format.FEN("SSSSS/S5/S6/S1sssss1/1S5s1/1SSSSS1s/6s/5s/sssss 0 0 b 0 0")
    val pieces    = snakesFen.pieces
    val board     = Board(
      pieces,
      History(
        score = Score(0, 0)
      ),
      variant.Abalone
    )
    val situation = Situation(board, P1)

    "have Black starting the game" in {
      snakesFen.player must_== Some(P1)
      snakesFen.value.split(' ').lift(3) must_== Some("b")
    }

    "have a total of 14 marbles per player" in {
      pieces.filter(p => p._2.player == P1).size must_== 14
      pieces.filter(p => p._2.player == P2).size must_== 14
    }

    "has an even number of valid moves, as the position is symmetrical" in {
      board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) % 2 must_== 0
    }
  }

  "Atomouche start position" should {
    val atomoucheFen = new format.FEN("Ss3/s2SsS/2S4/S1s4s/s2S1s2S/S4S1s/4s2/sSs2S/3Ss 0 0 b 0 0")
    val pieces       = atomoucheFen.pieces

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

  "Fun little game situation \"5/6/2S4/2SSSS2/2ssSs3/1sssSSS1/2S4/1ssS2/3s1 5 3 b 11 42\"" should {
    val puzzleFen = new format.FEN("5/6/2S4/2SSSS2/2ssSs3/1sssSSS1/2S4/1ssS2/3s1 5 3 b 11 42")
    val pieces    = puzzleFen.pieces
    val board     = Board(
      pieces,
      History(
        score = Score(5, 3)
      ),
      variant.Abalone
    )
    val situation = Situation(board, P1)

    "have a score of 5 for P1 and a score of 3 for P2" in {
      puzzleFen.player1Score must_== 5
      puzzleFen.player2Score must_== 3
    }

    "still have a total of 14 pieces per player if we include the ones pushed out" in {
      pieces.filter(p => p._2.player == P1).size + puzzleFen.player2Score must_== 14
      pieces.filter(p => p._2.player == P2).size + puzzleFen.player1Score must_== 14
    }

    "have a board containing 20 pieces" in {
      board.piecesOnBoardCount must_== 20
      board.piecesOf(P1).size + board.history.score.p2 must_== 14
      board.piecesOf(P2).size + board.history.score.p1 must_== 14
    }

    "11 plies were played since last time a marble was pushed out" in {
      puzzleFen.halfMovesSinceLastCapture must_== Some(11)
    }

    "42 moves were played in total" in {
      puzzleFen.fullMove must_== Some(42)
    }

    "board should be valid and have no winner" in {
      board.valid(true) must_== true
      situation.winner must_== None
      situation.status must_== None
    }
  }

  /*
   * * * * *
   * * S S S *
   * * * * s s s
   * * * * S S s s
   * * * S * S s s *
   * * s s * * * *
      S * * * * * *
   * * * * * *
   * * * * *
   */
  "Game just finished having FEN \"5/2sss1/4SSS/4ssSS/3s1sSS1/2SS4/s6/6/5 6 5 w 0 58\"" should {
    val fen       = format.FEN("5/2sss1/4SSS/4ssSS/3s1sSS1/2SS4/s6/6/5 6 5 w 0 58")
    val pieces    = fen.pieces
    val board     = Board(
      pieces,
      History(
        score = Score(6, 5)
      ),
      variant.Abalone
    )
    val situation = Situation(board, P2)

    "have a score of 6 for P1 and a score of 5 for P2" in {
      fen.player1Score must_== 6
      fen.player2Score must_== 5
    }

    "have a total of 14 marbles per player, on the board and pushed out" in {
      pieces.filter(p => p._2.player == P1).size + fen.player2Score must_== 14
      pieces.filter(p => p._2.player == P2).size + fen.player1Score must_== 14
    }

    // @TODO: ensure other types of moves are generated correctly when validMoves does work entirely
    "see 8 marbles able to move as 1" in {
      board.variant.validMovesOf1(situation).size must_== 8
    }
    // @TODO: ensure other types of moves are generated correctly when validMoves does work entirely
    "see 25 different moves of 1 marble" in {
      board.variant.validMovesOf1(situation).foldLeft(0)(_ + _._2.size) must_== 25
    }

    "but is ended and P1 is the winner" in {
      situation.end must_== true
      situation.playable(true) must_== false
      situation.stalemate must_== false
      situation.winner must_== Some(P1)
      situation.status must_== Some(Status.VariantEnd)
    }
  }

  "Game having a player unable to move" should {
    val board     = Board(
      format.FEN("PPPPP/PPPPPp/5Pp/6Pp/7Pp/7P/7/6/5 5 5 w 0 42").pieces,
      History(
        score = Score(5, 5)
      ),
      variant.Abalone
    )
    val situation = Situation(board, P2)

    // @TODO: ensure other types of moves are generated correctly when validMoves does work entirely
    "see no potential valid move for that player" in {
      board.variant.validMovesOf1(situation).size must_== 0
    }

    "end in a draw" in {
      situation.end must_== true
      situation.playable(true) must_== false
      situation.stalemate must_== true
      situation.winner must_== None
      situation.status must_== Some(Status.Stalemate)
    }
  }

  // @TODO: play a move triggering a game end because no marble was pushed out for too long :
  // "Game in progress since ages" should {
  //     val ongoingGame = new format.FEN("")

  //     "have a score of 3 for P2 and 2 for P1" in {
  //         fen.player1Score must_== 2
  //         fen.player2Score must_== 3
  //     }

  //     "be declared a draw" in {
  //       ... @TODO
  //      }
  // }
}
