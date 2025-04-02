package strategygames.dameo

import org.specs2.matcher.ValidatedMatchers

// import variant.Dameo

class DameoVariantTest extends DameoTest with ValidatedMatchers {
  "starting position" should {
    val fen = variant.Dameo.initialFen
    val board = Board(fen.pieces, variant.Dameo)
    val situation = Situation(board, P1)

    "have 52 opening moves" in {
      val moves = board.variant.validMoves(situation)
      moves.size must_== 52
    }

  }

/*
  "custom basic position" should {
    val fen       = format.FEN("5/6/7/8/4SSSs1/4s3/7/6/5 0 0 b 0 0")
    val board     = Board(fen.pieces, History(score = Score(0, 0)), variant.Dameo)
    val situation = Situation(board, P1)
    val movesOf1  = board.variant.validMovesOf1(situation)
    val movesOf23 = board.variant.validMovesOf2And3(situation)
    val game      = Game(
      board.variant
    ) // @TODO : check why it does not set the piecemap correctly. See a few lines below we have to use board.pieces instead.
    val validMoves  = game.board.variant.validMoves(situation)
    val game2       = game(validMoves(Pos.G5)(6))   // 3 marbles upLeft: g5e6
    val validMoves2 = game2.board.variant.validMoves(game2.situation)
    val game3       = game2(validMoves2(Pos.H5)(5)) // 1 marble downLeft h5g4
    val validMoves3 = game3.board.variant.validMoves(game3.situation)
    val game4       = game3(validMoves3(Pos.E6)(9)) // 3 marbles downRight e6g5
    val validMoves4 = game4.board.variant.validMoves(game4.situation)
    val game5       = game4(validMoves4(Pos.G4)(1)) // 1 marble upRight g4h5
    val validMoves5 = game5.board.variant.validMoves(game5.situation)
    val game6       = game5(validMoves5(Pos.F5)(7)) // 2 marbles downRight f5g4
    val validMoves6 = game6.board.variant.validMoves(game6.situation)

    "compute the correct number of moves of 1 marble" in {
      movesOf1.foldLeft(0)(_ + _._2.size) must_== 11
    }

    "compute the correct number of moves of 2 and 3 marbles" in {
      movesOf23.foldLeft(0)(_ + _._2.size) must_== (1 + 1 + 5) + (1 + 1 + 2)
    }
  }
*/ 
}