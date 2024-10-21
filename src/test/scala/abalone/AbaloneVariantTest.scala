package strategygames.abalone

import org.specs2.matcher.ValidatedMatchers

import _root_.strategygames.Score

class AbaloneVariantTest extends AbaloneTest with ValidatedMatchers {

    "valid Moves from custom basic position" should {
        val fen = format.FEN("5/6/7/8/4sssS1/4S3/7/6/5 0 0 b 0 0")
        val board = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
        val situation = Situation(board, P1)
        val movesOf1 = board.variant.validMovesOf1(situation)
        val movesOf2 = board.variant.validMovesOf2(situation, movesOf1)

        "compute the correct number of moves of 1 marble" in {
            movesOf1.foldLeft(0)(_ + _._2.size) must_== 11
        }

        "compute the following moves of 2 marbles : 1 push 1 line move and 5 side moves" in {
            movesOf2.foldLeft(0)(_ + _._2.size) must_== 1 + 1 + 5
        }        
    }

    "valid Moves from \"Belgian Daisy\" start position" should {
        val fen = variant.Abalone.initialFen
        val board = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
        val situation = Situation(board, P1)
        val movesOf1 = board.variant.validMovesOf1(situation)
        val movesOf2 = board.variant.validMovesOf2(situation, movesOf1)

        "compute the correct number of moves of 1 marble" in {
            movesOf1.foldLeft(0)(_ + _._2.size) must_== 20
        }

        "compute the correct number of moves of 2 marbles " in {
            movesOf2.foldLeft(0)(_ + _._2.size) must_== 28
        }
    }

    "valid Moves from \"Snakes\" start position" should {
        val fen = format.FEN("sssss/s5/s6/s1SSSSS1/1s5S1/1sssss1S/6S/5S/SSSSS 0 0 b 0 0")
        val board = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
        val situation = Situation(board, P1)
        val movesOf1 = board.variant.validMovesOf1(situation)
        val movesOf2 = board.variant.validMovesOf2(situation, movesOf1)

        "compute the correct number of moves of 1 marble" in {
            movesOf1.foldLeft(0)(_ + _._2.size) must_== 40
        }

        "compute the correct number of moves of 2 marbles" in { // 2 + 5 + 4 + 4 + 4 + 2 + 3 + 2 + 1 + 2 + 1 + 2 + 2 + 1
            movesOf2.foldLeft(0)(_ + _._2.size) must_== 35
        }        
    }

    "valid Moves from \"Alien Attack\" start position" should {
        val fen = format.FEN("s1s1s/1sSSs1/1sSsSs1/3ss3/9/3SS3/1SsSsS1/1SssS1/S1S1S 0 0 b 0 0")
        val board = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
        val situation = Situation(board, P1)
        val movesOf1 = board.variant.validMovesOf1(situation)
        val movesOf2 = board.variant.validMovesOf2(situation, movesOf1)

        "compute 28 moves of 1 marble" in {
            movesOf1.foldLeft(0)(_ + _._2.size) must_== 4 + 6 + 2 * (8) + 2
        }

        "compute the following moves of 2 marbles : 6 push 10 line moves and 4 side moves" in {
            movesOf2.foldLeft(0)(_ + _._2.size) must_== 6 + (2 * 2 + 4 + 2) + 4
        }        
    }

    "valid Moves from \"Domination\" start position" should {
        val fen = format.FEN("5/S4s/SS3ss/SSSS1sss/3s1s3/sss1SSSS/ss3SS/s4S/5 0 0 b 0 0")
        val board = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
        val situation = Situation(board, P1)
        val movesOf1 = board.variant.validMovesOf1(situation)
        val movesOf2 = board.variant.validMovesOf2(situation, movesOf1)

        "compute 28 moves of 1 marble" in {
            movesOf1.foldLeft(0)(_ + _._2.size) must_== 14 * 2
        }

        "compute the following moves of 2 marbles : 2 pushes 14 line moves and 18 side moves" in {
            movesOf2.foldLeft(0)(_ + _._2.size) must_== 2 * (1 + 7 + 9)
        }        
    }

    "valid Moves from custom start position with a total of 29 marbles" should {
        val fen = format.FEN("2S1S/3sS1/7/8/4sss2/SSss2s1/3S2s/2SsSS/1S3 5 5 b 0 0")
        val board = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
        val situation = Situation(board, P1)
        val movesOf1 = board.variant.validMovesOf1(situation)
        val movesOf2 = board.variant.validMovesOf2(situation, movesOf1)

        "compute 32 moves of 1 marble" in {
            movesOf1.foldLeft(0)(_ + _._2.size) must_== 3 + 4 + (4 + 3 + 4 + 4 + 4 + 4 + 2)
        }

        "compute the following moves of 2 marbles : 0 push 6 line moves and 14 side moves" in {
            movesOf2.foldLeft(0)(_ + _._2.size) must_== 6 + 14
        }

        val situationP2 = Situation(board, P2)
        val movesOf1P2 = board.variant.validMovesOf1(situationP2)
        val movesOf2P2 = board.variant.validMovesOf2(situationP2, movesOf1P2)        
        "for P2 : compute 31 moves of 1 marble" in {
            movesOf1P2.foldLeft(0)(_ + _._2.size) must_== 3 + 2 + 4 + 3 + 4 + 2 + 4 + 3 + 3 + 3
        }

        "for P2 : compute the following moves of 2 marbles : 0 push 2 line moves and 9 side moves" in {
            movesOf2P2.foldLeft(0)(_ + _._2.size) must_== 2 + (1 + 3 + 3 + 2)
        }
    }
}