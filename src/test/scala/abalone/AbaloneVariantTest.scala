package strategygames.abalone

import org.specs2.matcher.ValidatedMatchers

import _root_.strategygames.{ Score, Status }

class AbaloneVariantTest extends AbaloneTest with ValidatedMatchers {

    "custom basic position" should {
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

        "not have an intersection between moves of 1 marble and other moves" in {
            board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(_ + _._2.size) + movesOf23.foldLeft(0)(_ + _._2.size)
        }

        "compute the right set of moves of 2 and 3 marbles" in {
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

    "\"Belgian Daisy\" start position" should {
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

        "not have an intersection between moves of 1 marble and other moves" in {
            board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(_ + _._2.size) + movesOf23.foldLeft(0)(_ + _._2.size)
        }        
    }

    "\"Snakes\" start position" should {
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

        "not have an intersection between moves of 1 marble and other moves" in {
            board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(_ + _._2.size) + movesOf23.foldLeft(0)(_ + _._2.size)
        }
    }

    "\"Alien Attack\" start position" should {
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

        "not have an intersection between moves of 1 marble and other moves" in {
            board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(_ + _._2.size) + movesOf23.foldLeft(0)(_ + _._2.size)
        }
    }

    "\"Domination\" start position" should {
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

        "not have an intersection between moves of 1 marble and other moves" in {
            board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(_ + _._2.size) + movesOf23.foldLeft(0)(_ + _._2.size)
        }

        "find the moves pushing marbles" in {
            movesOf23.get(Pos.B3).get.map(x =>x._2.toString) should contain ( ("p1-Stone b3e6") )
            movesOf23.get(Pos.C4).get.map(x =>x._2.toString) should contain ( ("p1-Stone c4e6") )
            movesOf23.get(Pos.H7).get.map(x =>x._2.toString) should contain ( ("p1-Stone h7e4") )
            movesOf23.get(Pos.G6).get.map(x =>x._2.toString) should contain ( ("p1-Stone g6e4") )
        }
    }

    "custom start position with a total of 29 marbles" should {
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

        "not have an intersection between moves of 1 marble and other moves" in {
            board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(_ + _._2.size) + movesOf23.foldLeft(0)(_ + _._2.size)
        }

        val situationP2 = Situation(board, P2)
        val movesOf1P2 = board.variant.validMovesOf1(situationP2)
        val movesOf23P2 = board.variant.validMovesOf2And3(situationP2)        
        "for P2 : compute 31 moves of 1 marble" in {
            movesOf1P2.foldLeft(0)(_ + _._2.size) must_== 3 + 2 + 4 + 3 + 4 + 2 + 4 + 3 + 3 + 3
        }

        "for P2 : compute the correct number of moves of 2 and 3 marbles" in {
            movesOf23P2.foldLeft(0)(_ + _._2.size) must_== (0 + 2 + (1 + 3 + 3 + 2)) + (0 + 1 + 1)
        }
    }

    "custom start position" should {
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

        "not have an intersection between moves of 1 marble and other moves" in {
            board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(_ + _._2.size) + movesOf23.foldLeft(0)(_ + _._2.size)
        }

        "not generate a move for a marble that is blocked" in {
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

        "not generate a push when a same color marble prevents it (oooxo)" in {
            validMoves.get(Pos.E5).get.map(x =>x.toString) should not contain ("p1-Stone e5e2")
            validMoves.get(Pos.E5).get.map(x =>x.toString) should not contain ("p1-Stone e5e1")
        }

        "produce no move from a marble or a group or marbles that are stuck" in {
            validMoves.get(Pos.C1).get should beEmpty
            validMoves.get(Pos.D1).get should beEmpty
            validMoves.get(Pos.E1).get should beEmpty // (oooxo)
        }

        "not move more than 3 marbles" in {
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

        "not create a move in case of 2v2 push" in {
            validMoves.get(Pos.A2).get.map(x =>x.toString) should not contain ("p1-Stone a2a4")
            validMoves.get(Pos.A2).get.map(x =>x.toString) should not contain ("p1-Stone a2a5")
        }

        "not create a move in case of 3v3 push" in {
            validMoves.get(Pos.E4).get.map(x =>x.toString) should not contain ("p1-Stone e4e7")
            validMoves.get(Pos.E4).get.map(x =>x.toString) should not contain ("p1-Stone e4e8")
            validMoves.get(Pos.E4).get.map(x =>x.toString) should not contain ("p1-Stone e4e9")
        }
    }

    /*
            _ _ _ _ _
           _ _ _ _ _ _
          _ _ _ _ _ _ _
         _ _ _ _ _ _ _ _
        _ _ _ _ _ 0 0 0 *
         _ _ _ _ _ _ _ _
          _ _ _ _ _ _ _
           _ _ _ _ _ _
            _ _ _ _ _
    */
    "pushing out the only marble of P2 with a score of 0" should {
        val fenEndedGame = format.FEN("5/6/7/8/5sssS/8/7/6/5 0 0 b 0 0")
        val board = Board(fenEndedGame.pieces, History(score = Score(0, 0)), variant.Abalone)
        val situation = Situation(board, P1)
        val game = Game.apply(board.variant)
        val validMoves = game.board.variant.validMoves(situation)
        val game2 = game.apply(validMoves(Pos.F5)(5))

        "increment the score of P1" in {
            game2.situation.board.history.score must_== Score(1,0)
        }

        "trigger a stalemate" in {
            game2.situation.end must_== true
            game2.situation.staleMate must_== true
            game2.situation.playable(true) must_== false
            game2.situation.status must_== Some(Status.Stalemate)
            game2.situation.winner must_== None
        }
    }

    /*
            _ _ _ _ _
           _ _ _ _ _ _
          _ _ _ _ _ _ _
         _ _ _ _ _ _ _ _
        _ _ _ _ _ 0 0 0 *   5 - 0
         _ _ _ _ _ _ _ _
          _ _ _ _ _ _ _
           _ _ _ _ _ _
            _ _ _ _ _
    */
    "pushing out the only marble of P2 with as score of 5" should {
        val fenEndedGame = format.FEN("5/6/7/8/5sssS/8/7/6/5 5 0 b 0 0")
        val board = Board(fenEndedGame.pieces, History(score = Score(5, 0)), variant.Abalone)
        val situation = Situation(board, P1)
        val game = Game.apply(board.variant)
        val validMoves = game.board.variant.validMoves(situation)
        val game2 = game.apply(validMoves(Pos.F5)(5))

        "increment the score of P1" in {
            game2.situation.board.history.score must_== Score(6,0)
        }

        "trigger a win condition defined by the variant, even though it detects the stalemate" in {
            game2.situation.end must_== true
            game2.situation.staleMate must_== true
            game2.situation.playable(true) must_== false
            game2.situation.status must_== Some(Status.VariantEnd)
            game2.situation.winner must_== Some(P1)
        }
    }

    /*
            _ _ _ * *
           _ _ _ 0 * *
          _ _ _ _ 0 0 _
         _ _ _ _ _ _ 0 _
        _ _ _ _ 0 0 0 * *
         _ _ _ _ _ _ _ _
          _ _ _ _ _ _ _
           _ _ _ _ _ _
            * _ _ _ _
    */
    "pushing out 6 marbles consecutively" should {
        val fenEndedGame = format.FEN("3SS/3sSS/4ss1/6s1/4sssSS/8/7/6/S4 0 0 b 0 0")
        val board = Board(fenEndedGame.pieces, History(score = Score(0, 0)), variant.Abalone)
        val situation = Situation(board, P1)
        val game = Game.apply(board.variant)
        val validMoves = game.board.variant.validMoves(situation)
        val game2 = game.apply(validMoves(Pos.E5)(5)) // e5h5
        val validMoves2 = game2.board.variant.validMoves(game2.situation)
        val game3 = game2.apply(validMoves2(Pos.A1)(0)) // a1a2
        val validMoves3 = game3.board.variant.validMoves(game3.situation)
        val game4 = game3.apply(validMoves3(Pos.F5)(5)) // f5i5
        val validMoves4 = game4.board.variant.validMoves(game4.situation)
        val game5 = game4.apply(validMoves4(Pos.A2)(3)) // a2a1
        val validMoves5 = game5.board.variant.validMoves(game5.situation)
        val game6 = game5.apply(validMoves5(Pos.H5)(5)) // h5h8
        val validMoves6 = game6.board.variant.validMoves(game6.situation)
        val game7 = game6.apply(validMoves6(Pos.A1)(0)) // a1a2
        val validMoves7 = game7.board.variant.validMoves(game7.situation)
        val game8 = game7.apply(validMoves7(Pos.H6)(4)) // h6h9
        val validMoves8 = game8.board.variant.validMoves(game8.situation)
        val game9 = game8.apply(validMoves8(Pos.A2)(3)) // a2a1
        val validMoves9 = game9.board.variant.validMoves(game9.situation)
        val game10 = game9.apply(validMoves9(Pos.G8)(3)) // g8i8
        val validMoves10 = game10.board.variant.validMoves(game10.situation)
        val game11 = game10.apply(validMoves10(Pos.A1)(0)) // a1a2
        val validMoves11 = game11.board.variant.validMoves(game11.situation)
        val game12 = game11.apply(validMoves11(Pos.G7)(4)) // g7i9


        "increment the score of P1 each time he plays a move" in {
            game2.situation.board.history.score must_== Score(1,0)
            game4.situation.board.history.score must_== Score(2,0)
            game6.situation.board.history.score must_== Score(3,0)
            game8.situation.board.history.score must_== Score(4,0)
            game10.situation.board.history.score must_== Score(5,0)
            game12.situation.board.history.score must_== Score(6,0)
        }

        "trigger a win condition defined by the variant, and no stalemate is detected" in {
            game12.situation.end must_== true
            game12.situation.staleMate must_== false
            game12.situation.playable(true) must_== false
            game12.situation.status must_== Some(Status.VariantEnd)
            game12.situation.winner must_== Some(P1)
        }
    }
}