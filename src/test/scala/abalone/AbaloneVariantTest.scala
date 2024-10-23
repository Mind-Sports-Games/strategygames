package strategygames.abalone

import org.specs2.matcher.ValidatedMatchers

import _root_.strategygames.Score

class AbaloneVariantTest extends AbaloneTest with ValidatedMatchers {

    "valid Moves from custom basic position" should {
        val fen = format.FEN("5/6/7/8/4sssS1/4S3/7/6/5 0 0 b 0 0")
        val board = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
        val situation = Situation(board, P1)
        val movesOf1 = board.variant.validMovesOf1(situation)
        val movesOf23 = board.variant.validMovesOf2And3(situation)

        "compute the correct number of moves of 1 marble" in {
            movesOf1.foldLeft(0)(_ + _._2.size) must_== 11
        }

        "compute the correct number of moves of 2 and 3 marbles" in {
            movesOf23.foldLeft(0)(_ + _._2.size) must_== (1 + 1 + 5) + (1 + 1 + 2)
        }

        "moves of 1 marble and other moves do not intersect" in {
            board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(_ + _._2.size) + movesOf23.foldLeft(0)(_ + _._2.size)
        }

        "compute the moves of 2 and 3 marbles" in {
            movesOf23.get(Pos.E5).get.map(x =>x._2.toUci.toString) should contain ( ("Move(e5,h5,None)"))
            movesOf23.get(Pos.E5).get.map(x =>x._2.toUci.toString) should contain ( ("Move(e5,g6,None)"))
            movesOf23.get(Pos.E5).get.map(x =>x._2.toUci.toString) should contain ( ("Move(e5,h6,None)"))

            movesOf23.get(Pos.F5).get.map(x =>x._2.toUci.toString) should contain ( ("Move(f5,d5,None)"))
            movesOf23.get(Pos.F5).get.map(x =>x._2.toUci.toString) should contain ( ("Move(f5,e6,None)"))
            movesOf23.get(Pos.F5).get.map(x =>x._2.toUci.toString) should contain ( ("Move(f5,h5,None)"))
            movesOf23.get(Pos.F5).get.map(x =>x._2.toUci.toString) should contain ( ("Move(f5,h6,None)"))
            movesOf23.get(Pos.F5).get.map(x =>x._2.toUci.toString) should contain ( ("Move(f5,g4,None)"))

            movesOf23.get(Pos.G5).get.map(x =>x._2.toUci.toString) should contain ( ("Move(g5,d5,None)"))
            movesOf23.get(Pos.G5).get.map(x =>x._2.toUci.toString) should contain ( ("Move(g5,e6,None)"))
            movesOf23.get(Pos.G5).get.map(x =>x._2.toUci.toString) should contain ( ("Move(g5,f6,None)"))
        }
    }

    "valid Moves from \"Belgian Daisy\" start position" should {
        val fen = variant.Abalone.initialFen
        val board = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
        val situation = Situation(board, P1)
        val movesOf1 = board.variant.validMovesOf1(situation)
        val movesOf23 = board.variant.validMovesOf2And3(situation)

        "compute the correct number of moves of 1 marble" in {
            movesOf1.foldLeft(0)(_ + _._2.size) must_== 20
        }

        "compute the correct number of moves of 2 and 3 marbles" in {
            movesOf23.foldLeft(0)(_ + _._2.size) must_== 28 + 4
            movesOf23.get(Pos.A1).get.size must_== 3
            movesOf23.get(Pos.A2).get.size must_== 2
            movesOf23.get(Pos.B1).get.size must_== 2
            movesOf23.get(Pos.B2).get.size must_== 2
            movesOf23.get(Pos.C2).get.size must_== 2
            movesOf23.get(Pos.B3).get.size must_== 2
            movesOf23.get(Pos.C3).get.size must_== 3
        }

        "moves of 1 marble and other moves do not intersect" in {
            board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(_ + _._2.size) + movesOf23.foldLeft(0)(_ + _._2.size)
        }        
    }

    "valid Moves from \"Snakes\" start position" should {
        val fen = format.FEN("sssss/s5/s6/s1SSSSS1/1s5S1/1sssss1S/6S/5S/SSSSS 0 0 b 0 0")
        val board = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
        val situation = Situation(board, P1)
        val movesOf1 = board.variant.validMovesOf1(situation)
        val movesOf23 = board.variant.validMovesOf2And3(situation)

        "compute the correct number of moves of 1 marble" in {
            movesOf1.foldLeft(0)(_ + _._2.size) must_== 40
        }

        "compute the correct number of moves of 2 and 3 marbles" in {
            movesOf23.foldLeft(0)(_ + _._2.size) must_==
                (2 + 5 + 4 + 4 + 4 + 2 + 3 + 2 + 1 + 2 + 1 + 2 + 2 + 1) + 
                (0 + 4 + (2 + 2 + 3 + 2 + 2 + 2 + 2 + 1 + 1 + 1 + 1))
        }

        "moves of 1 marble and other moves do not intersect" in {
            board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(_ + _._2.size) + movesOf23.foldLeft(0)(_ + _._2.size)
        }
    }

    "valid Moves from \"Alien Attack\" start position" should {
        val fen = format.FEN("s1s1s/1sSSs1/1sSsSs1/3ss3/9/3SS3/1SsSsS1/1SssS1/S1S1S 0 0 b 0 0")
        val board = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
        val situation = Situation(board, P1)
        val movesOf1 = board.variant.validMovesOf1(situation)
        val movesOf23 = board.variant.validMovesOf2And3(situation)

        "compute 28 moves of 1 marble" in {
            movesOf1.foldLeft(0)(_ + _._2.size) must_== 4 + 6 + 2 * (8) + 2
        }

        "compute the correct number of moves of 2 and 3 marbles" in {
            movesOf23.foldLeft(0)(_ + _._2.size) must_== 6 + (2 * 2 + 4 + 2) + 4
        }

        "moves of 1 marble and other moves do not intersect" in {
            board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(_ + _._2.size) + movesOf23.foldLeft(0)(_ + _._2.size)
        }
    }

    "valid Moves from \"Domination\" start position" should {
        val fen = format.FEN("5/S4s/SS3ss/SSSS1sss/3s1s3/sss1SSSS/ss3SS/s4S/5 0 0 b 0 0")
        val board = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
        val situation = Situation(board, P1)
        val movesOf1 = board.variant.validMovesOf1(situation)
        val movesOf23 = board.variant.validMovesOf2And3(situation)

        "compute 28 moves of 1 marble" in {
            movesOf1.foldLeft(0)(_ + _._2.size) must_== 14 * 2
        }

        "compute the correct number of moves of 2 and 3 marbles" in {
            movesOf23.foldLeft(0)(_ + _._2.size) must_== 2 * (1 + 7 + 9) + 2 * (1 + 3 + 5)
        }

        "moves of 1 marble and other moves do not intersect" in {
            board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(_ + _._2.size) + movesOf23.foldLeft(0)(_ + _._2.size)
        }

        "find the valid pushes for P1" in {
            movesOf23.get(Pos.B3).get.map(x =>x._2.toString) should contain ( ("p1-Stone b3e6") )
            movesOf23.get(Pos.C4).get.map(x =>x._2.toString) should contain ( ("p1-Stone c4e6") )
            movesOf23.get(Pos.H7).get.map(x =>x._2.toString) should contain ( ("p1-Stone h7e4") )
            movesOf23.get(Pos.G6).get.map(x =>x._2.toString) should contain ( ("p1-Stone g6e4") )
        }
    }

    "valid Moves from custom start position with a total of 29 marbles" should {
        val fen = format.FEN("2S1S/3sS1/7/8/4sss2/SSss2s1/3S2s/2SsSS/1S3 5 5 b 0 0")
        val board = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
        val situation = Situation(board, P1)
        val movesOf1 = board.variant.validMovesOf1(situation)
        val movesOf23 = board.variant.validMovesOf2And3(situation)

        "compute 32 moves of 1 marble" in {
            movesOf1.foldLeft(0)(_ + _._2.size) must_== 3 + 4 + (4 + 3 + 4 + 4 + 4 + 4 + 2)
        }

        "compute the correct number of moves of 2 and 3 marbles" in {
            movesOf23.foldLeft(0)(_ + _._2.size) must_== (0 + 6 + 14) + (0 + 3 + 3) 
        }

        "moves of 1 marble and other moves do not intersect" in {
            board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(_ + _._2.size) + movesOf23.foldLeft(0)(_ + _._2.size)
        }

        val situationP2 = Situation(board, P2)
        val movesOf1P2 = board.variant.validMovesOf1(situationP2)
        val movesOf23P2 = board.variant.validMovesOf2And3(situationP2)        
        "for P2 : compute 31 moves of 1 marble" in {
            movesOf1P2.foldLeft(0)(_ + _._2.size) must_== 3 + 2 + 4 + 3 + 4 + 2 + 4 + 3 + 3 + 3
        }

        "for P2 : compute the following moves of 2 marbles : 0 push 2 line moves and 9 side moves" in {
            movesOf23P2.foldLeft(0)(_ + _._2.size) must_== (0 + 2 + (1 + 3 + 3 + 2)) + (0 + 1 + 1)
        }
    }

    "valid Moves from custom start position" should {
        val fen = format.FEN("sss2/2Ssss/1ssSSSS/3s4/3s5/2s5/1S5/SSSS2/ssssS 5 5 b 0 0")
        val board = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
        val situation = Situation(board, P1)
        val movesOf1 = board.variant.validMovesOf1(situation)
        val movesOf23 = board.variant.validMovesOf2And3(situation)

        "compute 32 moves of 1 marble" in {
            movesOf1.foldLeft(0)(_ + _._2.size) must_== 2 + 1 + 1 + 1 + 2 + 1 + 5 + 2 + 3 + 4 + 4 + 1
        }

        "compute the correct number of moves of 2 and 3 marbles" in {
            movesOf23.foldLeft(0)(_ + _._2.size) must_== (3 + 4 + (1 + 1 + 1 + 2 + 2 + 3 + 2)) + (3 + 1 + 3)
        }

        "moves of 1 marble and other moves do not intersect" in {
            board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(_ + _._2.size) + movesOf23.foldLeft(0)(_ + _._2.size)
        }

        "do not generate a move for a marble that is blocked" in {
            movesOf1.get(Pos.A1).get should beEmpty
            movesOf23.get(Pos.A1).get should beEmpty
        }

        "find and compute all moves that push off (3v2, 3v1, 2v1)" in {
            movesOf23.get(Pos.B1).get.map(x =>x._2.toUci.toString) should contain ( ("Move(b1,e1,None)") )
            movesOf23.get(Pos.C1).get.map(x =>x._2.toUci.toString) should contain ( ("Move(c1,e1,None)") )
            movesOf23.get(Pos.E6).get.map(x =>x._2.toUci.toString) should contain ( ("Move(e6,b3,None)") )
        }
    }

    "special position to test some edge cases" should {
        val fen = format.FEN("S4/sS4/s1S4/s2s4/S3s4/S3s3/s3s2/sSSSSS/sSsss 5 5 b 0 0")
        val board = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
        val situation = Situation(board, P1)
        val validMoves = board.variant.validMoves(situation);

        "do not generate a push when a same color marble prevents it (oooxo)" in {
            validMoves.get(Pos.E5).get.map(x =>x.toString) should not contain ("p1-Stone e5e2")
            validMoves.get(Pos.E5).get.map(x =>x.toString) should not contain ("p1-Stone e5e1")
        }

        "produce no move from a marble or a group or marbles that are stuck" in {
            validMoves.get(Pos.C1).get should beEmpty
            validMoves.get(Pos.D1).get should beEmpty
            validMoves.get(Pos.E1).get should beEmpty // (oooxo)
        }

        "do not move more than 3 marbles" in {
            validMoves.get(Pos.E3).get.map(x =>x.toString) should not contain ("p1-Stone e3e7")
            validMoves.get(Pos.E3).get.map(x =>x.toString) should not contain ("p1-Stone e3e8")
            validMoves.get(Pos.E3).get.map(x =>x.toString) should not contain ("p1-Stone e3e9")
            validMoves.get(Pos.E3).get.map(x =>x.toString) should not contain ("p1-Stone e3d6")
            validMoves.get(Pos.E3).get.map(x =>x.toString) should not contain ("p1-Stone e3f7")
            validMoves.get(Pos.E3).get.size must_== 7
        }

        "find and compute all moves that push off (3v2, 3v1, 2v1)" in {
            validMoves.get(Pos.D8).get.map(x =>x.toString) should contain ("p1-Stone d8a5")
            validMoves.get(Pos.C7).get.map(x =>x.toString) should contain ("p1-Stone c7a5")
            validMoves.get(Pos.C7).get.map(x =>x.toString) should contain ("p1-Stone c7e9")
            validMoves.get(Pos.B6).get.map(x =>x.toString) should contain ("p1-Stone b6e9")

            validMoves.get(Pos.A1).get.map(x =>x.toString) should contain ("p1-Stone a1a4")
            validMoves.get(Pos.A1).get.map(x =>x.toString) should not contain ("p1-Stone a1a5")
        }

        "do not push 2v2" in {
            validMoves.get(Pos.A2).get.map(x =>x.toString) should not contain ("p1-Stone a2a4")
            validMoves.get(Pos.A2).get.map(x =>x.toString) should not contain ("p1-Stone a2a5")
        }

        "do not push 3v3" in {
            validMoves.get(Pos.E4).get.map(x =>x.toString) should not contain ("p1-Stone e4e7")
            validMoves.get(Pos.E4).get.map(x =>x.toString) should not contain ("p1-Stone e4e8")
            validMoves.get(Pos.E4).get.map(x =>x.toString) should not contain ("p1-Stone e4e9")
        }
    }
}