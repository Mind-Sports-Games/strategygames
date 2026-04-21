package strategygames.abalone

import strategygames.abalone.variant.Abalone
import strategygames.{ Score, Status }

class AbaloneFenTest extends AbaloneTest {
  "initial default FEN (Belgian daisy start position)" should {
    val fen        = Abalone.initialFen
    val pieces     = fen.pieces(Abalone)
    val board      = Board(pieces, History(), Abalone)
    val situation  = Situation(board, P1)
    def validMoves = valid(situation)

    "have Black starting the game" in {
      fen.player === Some(P1)
      fen.value.split(' ').lift(3) === Some("b")
    }

    "have a total of 14 marbles per player" in {
      pieces.filter(p => p._2.player == P1).size === 14
      pieces.filter(p => p._2.player == P2).size === 14
    }

    "set the score to zero for each player" in {
      fen.player1Score === 0
      fen.player2Score === 0
    }

    "set both moves counters to expected initial value" in {
      fen.fullMove.get === 1
      fen.halfMovesSinceLastCapture.get === 0
    }

    "draw a daisy of 7 marbles side by side on bottom for each player, then applies central symmetry on e5 to draw the ones on the top" in {
      pieces.get(new Pos(0, 0)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(1, 0)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(3, 0)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(4, 0)) === Some(Piece(P2, Stone))

      pieces.get(new Pos(0, 1)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(1, 1)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(2, 1)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(3, 1)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(4, 1)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(5, 1)) === Some(Piece(P2, Stone))

      pieces.get(new Pos(1, 2)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(2, 2)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(4, 2)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(5, 2)) === Some(Piece(P2, Stone))

      pieces.get(new Pos(3, 6)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(4, 6)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(6, 6)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(7, 6)) === Some(Piece(P1, Stone))

      pieces.get(new Pos(3, 7)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(4, 7)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(5, 7)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(6, 7)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(7, 7)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(8, 7)) === Some(Piece(P1, Stone))

      pieces.get(new Pos(4, 8)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(5, 8)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(7, 8)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(8, 8)) === Some(Piece(P1, Stone))
    }

    // @TODO: ensure other types of moves are generated correctly when validMoves does work entirely
    "see 10 marbles able to move as 1" in {
      validMoves.filter(_._2.find(of1(board)).isDefined).size === 10
    }
    // @TODO: ensure other types of moves are generated correctly when validMoves does work entirely
    "see 20 different moves of 1 marble" in {
      validMoves.flatMap(_._2).filter(of1(board)).size === 20
    }
  }

  "Snakes variant start position" should {
    val snakesVariantFen = new format.FEN("SSSSS/S5/S6/S1sssss1/1S5s1/1SSSSS1s/6s/5s/sssss 0 0 b 0 0")
    val pieces           = snakesVariantFen.pieces(Abalone)
    val board            = Board(pieces, History(), Abalone)
    val situation        = Situation(board, P1)

    "have Black starting the game" in {
      snakesVariantFen.player === Some(P1)
      snakesVariantFen.value.split(' ').lift(3) === Some("b")
    }

    "have a total of 14 marbles per player" in {
      pieces.filter(_._2.player == P1).size === 14
      pieces.filter(_._2.player == P2).size === 14
    }

    "have an even number of valid moves, as the position is symmetrical" in {
      board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) % 2 === 0

    }
  }

  "L'Atomouche start position" should {
    val atomoucheFen = new format.FEN("Ss3/s2SsS/2S4/S1s4s/s2S1s2S/S4S1s/4s2/sSs2S/3Ss 0 0 b 0 0")
    val pieces       = atomoucheFen.pieces(Abalone)

    "have Black starting the game" in {
      atomoucheFen.player === Some(P1)
      atomoucheFen.value.split(' ').lift(3) === Some("b")
    }

    "have a total of 12 marbles per player" in {
      pieces.filter(p => p._2.player == P1).size === 12
      pieces.filter(p => p._2.player == P2).size === 12
    }

    "have each marble placed at the expected position" in {
      pieces.get(new Pos(3, 0)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(1, 1)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(5, 1)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(0, 3)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(5, 3)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(3, 4)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(8, 4)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(1, 5)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(4, 6)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(6, 7)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(8, 7)) === Some(Piece(P1, Stone))
      pieces.get(new Pos(4, 8)) === Some(Piece(P1, Stone))

      pieces.get(new Pos(4, 0)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(0, 1)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(2, 1)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(4, 2)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(7, 3)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(0, 4)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(5, 4)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(3, 5)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(8, 5)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(3, 7)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(7, 7)) === Some(Piece(P2, Stone))
      pieces.get(new Pos(5, 8)) === Some(Piece(P2, Stone))
    }
  }

  "Fun little game situation \"5/6/2S4/2SSSS2/2ssSs3/1sssSSS1/2S4/1ssS2/3s1 5 3 b 11 42\"" should {
    val puzzleFen = new format.FEN("5/6/2S4/2SSSS2/2ssSs3/1sssSSS1/2S4/1ssS2/3s1 5 3 b 11 42")
    val pieces    = puzzleFen.pieces(Abalone)
    val board     = Board(pieces, History(score = Score(5, 3)), Abalone)
    val situation = Situation(board, P1)

    "have a score of 5 for P1 and a score of 3 for P2" in {
      puzzleFen.player1Score === 5
      puzzleFen.player2Score === 3
    }

    "still have a total of 14 pieces per player if we include the ones pushed out" in {
      pieces.filter(p => p._2.player == P1).size + puzzleFen.player2Score === 14
      pieces.filter(p => p._2.player == P2).size + puzzleFen.player1Score === 14
    }

    "have a board containing 20 pieces" in {
      board.piecesOnBoardCount === 20
      board.piecesOf(P1).size + board.history.score.p2 === 14
      board.piecesOf(P2).size + board.history.score.p1 === 14
    }

    "11 plies were played since last time a marble was pushed out" in {
      puzzleFen.halfMovesSinceLastCapture === Some(11)
    }

    "42 moves were played in total" in {
      puzzleFen.fullMove === Some(42)
    }

    "board should be valid and have no winner" in {
      board.valid(true) === true
      situation.winner === None
      situation.status === None
    }
  }

  /*
   *     · · · · ·
   *    · · o o o ·
   *   · · · · ● ● ●
   *  · · · · o o ● ●
   * · · · o · o ● ● ·
   *  · · ● ● · · · ·
   *   o · · · · · ·
   *    · · · · · ·
   *     · · · · ·
   */
  "Game just finished having FEN \"5/2sss1/4SSS/4ssSS/3s1sSS1/2SS4/s6/6/5 6 5 w 0 58\"" should {
    val fen        = format.FEN("5/2sss1/4SSS/4ssSS/3s1sSS1/2SS4/s6/6/5 6 5 w 0 58")
    val pieces     = fen.pieces(Abalone)
    val board      = Board(pieces, History(score = Score(6, 5)), Abalone)
    val situation  = Situation(board, P2)
    val validMoves = valid(situation)

    "have a score of 6 for P1 and a score of 5 for P2" in {
      fen.player1Score === 6
      fen.player2Score === 5
    }

    "have a total of 14 marbles per player, on the board and pushed out" in {
      pieces.filter(_._2.player == P1).size + fen.player2Score === 14
      pieces.filter(_._2.player == P2).size + fen.player1Score === 14
    }

    // @TODO: ensure other types of moves are generated correctly when validMoves does work entirely
    "see 7 marbles able to move as 1" in {
      validMoves.filter(_._2.find(of1(board)).isDefined).size === 7
    }
    // @TODO: ensure other types of moves are generated correctly when validMoves does work entirely
    "see 25 different moves of 1 marble" in {
      validMoves.flatMap(_._2).filter(of1(board)).size === 25
    }

    "but is ended and P1 is the winner" in {
      situation.end === true
      situation.playable(true) === false
      situation.staleMate === false
      situation.winner === Some(P1)
      situation.status === Some(Status.VariantEnd)
    }
  }

  "Game having a player unable to move" should {
    val board     = Board(
      format.FEN("SSSSS/SSSSSS/5SS/6SS/7SS/7S/7/6/5 5 5 w 0 42").pieces(Abalone),
      History(score = Score(5, 5)),
      Abalone
    )
    val situation = Situation(board, P2)

    // @TODO: ensure other types of moves are generated correctly when validMoves does work entirely
    "see no potential valid move for that player" in {
      valid(situation).size === 0
    }

    "end in a loss for the player unable to move" in {
      situation.end === true
      situation.playable(true) === false
      situation.staleMate === false
      situation.winner === Some(P1)
      situation.status === Some(Status.VariantEnd)
    }
  }

  // @TODO: play a move triggering a game end because no marble was pushed out for too long :
  // "Game in progress since ages" should {
  //     val ongoingGame = new format.FEN("")

  //     "have a score of 3 for P2 and 2 for P1" in {
  //         fen.player1Score === 2
  //         fen.player2Score === 3
  //     }

  //     "be declared a draw" in {
  //       ... @TODO
  //      }
  // }
}
