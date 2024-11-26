package strategygames.abalone

import org.specs2.matcher.ValidatedMatchers

import strategygames.{ Score, Status }
import strategygames.format.{ FEN => StratFen, Forsyth => StratForsyth, Uci => StratUci }
import strategygames.variant.{ Variant => StratVariant }
import variant.Abalone

class AbaloneVariantTest extends AbaloneTest with ValidatedMatchers {
  /*
            _ _ _ _ _
           _ _ _ _ _ _
          _ _ _ _ _ _ _
         _ _ _ _ _ _ _ _
        _ _ _ _ 0 0 0 * _
         _ _ _ _ * _ _ _
          _ _ _ _ _ _ _
           _ _ _ _ _ _
            _ _ _ _ _
        P1: E5, F5, G5.
        P2. E4, H5
   */
  "custom basic position" should {
    val fen       = format.FEN("5/6/7/8/4SSSs1/4s3/7/6/5 0 0 b 0 0")
    val board     = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
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

    "not have an intersection between moves of 1 marble and other moves" in {
      board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(
        _ + _._2.size
      ) + movesOf23.foldLeft(0)(_ + _._2.size)
    }

    "compute the right set of moves of 2 and 3 marbles" in {
      movesOf23.get(Pos.E5).get.map(x => x.toUci.toString) should contain("Move(e5,h5)")
      movesOf23.get(Pos.E5).get.map(x => x.toUci.toString) should contain("Move(e5,g6)")
      movesOf23.get(Pos.E5).get.map(x => x.toUci.toString) should contain("Move(e5,h6)")

      movesOf23.get(Pos.F5).get.map(x => x.toUci.toString) should contain("Move(f5,d5)")
      movesOf23.get(Pos.F5).get.map(x => x.toUci.toString) should contain("Move(f5,e6)")
      movesOf23.get(Pos.F5).get.map(x => x.toUci.toString) should contain("Move(f5,h5)")
      movesOf23.get(Pos.F5).get.map(x => x.toUci.toString) should contain("Move(f5,h6)")
      movesOf23.get(Pos.F5).get.map(x => x.toUci.toString) should contain("Move(f5,g4)")

      movesOf23.get(Pos.G5).get.map(x => x.toUci.toString) should contain("Move(g5,d5)")
      movesOf23.get(Pos.G5).get.map(x => x.toUci.toString) should contain("Move(g5,e6)")
      movesOf23.get(Pos.G5).get.map(x => x.toUci.toString) should contain("Move(g5,f6)")
    }

    "side moves of 3 do move 3 marbles" in {
      game2.situation.board.pieces.contains(Pos.E6) must_== true
      game2.situation.board.pieces.contains(Pos.F6) must_== true
      game2.situation.board.pieces.contains(Pos.G6) must_== true
      game2.situation.board.pieces.contains(Pos.E4) must_== true
      game2.situation.board.pieces.contains(Pos.H5) must_== true
      game2.situation.board.pieces.size must_== 5
      validMoves2(Pos.E4).map(x => x.toUci.toString) should contain("Move(e4,e5)")
      validMoves2(Pos.E4).map(x => x.toUci.toString) should contain("Move(e4,f5)")
      validMoves2(Pos.H5).map(x => x.toUci.toString) should contain("Move(h5,g5)")

      board.pieces must_== game5.situation.board.pieces
      game5.situation.board.pieces.size must_== 5
    }

    "side moves of 2 do move 2 marbles" in {
      game6.situation.board.pieces.contains(Pos.E5) must_== true
      game6.situation.board.pieces.contains(Pos.F4) must_== true
      game6.situation.board.pieces.contains(Pos.G4) must_== true
      game6.situation.board.pieces.contains(Pos.E4) must_== true
      game6.situation.board.pieces.contains(Pos.H5) must_== true
      game6.situation.board.pieces.size must_== 5
      validMoves6(Pos.E4).map(x => x.toUci.toString) should not contain "Move(e4,f4)"
      validMoves6(Pos.E4).map(x => x.toUci.toString) should not contain "Move(e4,e5)"
      validMoves6(Pos.E4).map(x => x.toUci.toString) should contain("Move(e4,f5)")
      validMoves6(Pos.H5).map(x => x.toUci.toString) should contain("Move(h5,g5)")
    }
  }

  /*
            0 0 _ * *
           0 0 0 * * *
          _ 0 0 _ * * _
         _ _ _ _ _ _ _ _
        _ _ _ _ _ _ _ _ _
         _ _ _ _ _ _ _ _
          _ * * _ 0 0 _
   * * * 0 0 0
   * * _ 0 0
   */
  "\"Belgian Daisy\" start position" should {
    val fen       = variant.Abalone.initialFen
    val board     = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
    val situation = Situation(board, P1)
    val movesOf1  = board.variant.validMovesOf1(situation)
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
      board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(
        _ + _._2.size
      ) + movesOf23.foldLeft(0)(_ + _._2.size)
    }
  }

  "\"Snakes\" start position" should {
    val fen       = format.FEN("SSSSS/S5/S6/S1sssss1/1S5s1/1SSSSS1s/6s/5s/sssss 0 0 b 0 0")
    val board     = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
    val situation = Situation(board, P1)
    val movesOf1  = board.variant.validMovesOf1(situation)
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
      board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(
        _ + _._2.size
      ) + movesOf23.foldLeft(0)(_ + _._2.size)
    }
  }

  "\"Alien Attack\" start position" should {
    val fen       = format.FEN("S1S1S/1SssS1/1SsSsS1/3SS3/9/3ss3/1sSsSs1/1sSSs1/s1s1s 0 0 b 0 0")
    val board     = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
    val situation = Situation(board, P1)
    val movesOf1  = board.variant.validMovesOf1(situation)
    val movesOf23 = board.variant.validMovesOf2And3(situation)

    "compute 28 moves of 1 marble" in {
      movesOf1.foldLeft(0)(_ + _._2.size) must_== 4 + 6 + 2 * 8 + 2
    }

    "compute the correct number of moves of 2 and 3 marbles" in {
      movesOf23.foldLeft(0)(_ + _._2.size) must_== 6 + (2 * 2 + 4 + 2) + 4
    }

    "not have an intersection between moves of 1 marble and other moves" in {
      board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(
        _ + _._2.size
      ) + movesOf23.foldLeft(0)(_ + _._2.size)
    }
  }

  "\"Domination\" start position" should {
    val fen       = format.FEN("5/s4S/ss3SS/ssss1SSS/3S1S3/SSS1ssss/SS3ss/S4s/5 0 0 b 0 0")
    val board     = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
    val situation = Situation(board, P1)
    val movesOf1  = board.variant.validMovesOf1(situation)
    val movesOf23 = board.variant.validMovesOf2And3(situation)

    "compute 28 moves of 1 marble" in {
      movesOf1.foldLeft(0)(_ + _._2.size) must_== 14 * 2
    }

    "compute the correct number of moves of 2 and 3 marbles" in {
      movesOf23.foldLeft(0)(_ + _._2.size) must_== 2 * (1 + 7 + 9) + 2 * (1 + 3 + 5)
    }

    "not have an intersection between moves of 1 marble and other moves" in {
      board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(
        _ + _._2.size
      ) + movesOf23.foldLeft(0)(_ + _._2.size)
    }

    "find the moves pushing marbles" in {
      movesOf23.get(Pos.B3).get.map(x => x.toString) should contain("p1-Stone b3e6")
      movesOf23.get(Pos.C4).get.map(x => x.toString) should contain("p1-Stone c4e6")
      movesOf23.get(Pos.H7).get.map(x => x.toString) should contain("p1-Stone h7e4")
      movesOf23.get(Pos.G6).get.map(x => x.toString) should contain("p1-Stone g6e4")
    }
  }

  "custom start position with a total of 29 marbles" should {
    val fen       = format.FEN("2s1s/3Ss1/7/8/4SSS2/ssSS2S1/3s2S/2sSss/1s3 5 5 b 0 0")
    val board     = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
    val situation = Situation(board, P1)
    val movesOf1  = board.variant.validMovesOf1(situation)
    val movesOf23 = board.variant.validMovesOf2And3(situation)

    "compute 32 moves of 1 marble" in {
      movesOf1.foldLeft(0)(_ + _._2.size) must_== 3 + 4 + (4 + 3 + 4 + 4 + 4 + 4 + 2)
    }

    "compute the correct number of moves of 2 and 3 marbles" in {
      movesOf23.foldLeft(0)(_ + _._2.size) must_== (0 + 6 + 14) + (0 + 3 + 3)
    }

    "not have an intersection between moves of 1 marble and other moves" in {
      board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(
        _ + _._2.size
      ) + movesOf23.foldLeft(0)(_ + _._2.size)
    }

    val situationP2 = Situation(board, P2)
    val movesOf1P2  = board.variant.validMovesOf1(situationP2)
    val movesOf23P2 = board.variant.validMovesOf2And3(situationP2)
    "for P2 : compute 31 moves of 1 marble" in {
      movesOf1P2.foldLeft(0)(_ + _._2.size) must_== 3 + 2 + 4 + 3 + 4 + 2 + 4 + 3 + 3 + 3
    }

    "for P2 : compute the correct number of moves of 2 and 3 marbles" in {
      movesOf23P2.foldLeft(0)(_ + _._2.size) must_== (0 + 2 + (1 + 3 + 3 + 2)) + (0 + 1 + 1)
    }
  }

  "custom start position" should {
    val fen       = format.FEN("SSS2/2sSSS/1SSssss/3S4/3S5/2S5/1s5/ssss2/SSSSs 5 5 b 0 0")
    val board     = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
    val situation = Situation(board, P1)
    val movesOf1  = board.variant.validMovesOf1(situation)
    val movesOf23 = board.variant.validMovesOf2And3(situation)

    "compute 32 moves of 1 marble" in {
      movesOf1.foldLeft(0)(_ + _._2.size) must_== 2 + 1 + 1 + 1 + 2 + 1 + 5 + 2 + 3 + 4 + 4 + 1
    }

    "compute the correct number of moves of 2 and 3 marbles" in {
      movesOf23.foldLeft(0)(_ + _._2.size) must_== (3 + 4 + (1 + 1 + 1 + 2 + 2 + 3 + 2)) + (3 + 1 + 3)
    }

    "not have an intersection between moves of 1 marble and other moves" in {
      board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(
        _ + _._2.size
      ) + movesOf23.foldLeft(0)(_ + _._2.size)
    }

    "not generate a move for a marble that is blocked" in {
      movesOf1.get(Pos.A1).get should beEmpty
      movesOf23.get(Pos.A1).get should beEmpty
    }

    "find and compute all moves that push off (3v2, 3v1, 2v1)" in {
      movesOf23.get(Pos.B1).get.map(x => x.toUci.toString) should contain("Move(b1,e1)")
      movesOf23.get(Pos.C1).get.map(x => x.toUci.toString) should contain("Move(c1,e1)")
      movesOf23.get(Pos.E6).get.map(x => x.toUci.toString) should contain("Move(e6,b3)")
    }
  }

  "special position to test some edge cases" should {
    val fen        = format.FEN("s4/Ss4/S1s4/S2S4/s3S4/s3S3/S3S2/Ssssss/SsSSS 5 5 b 0 0")
    val board      = Board(fen.pieces, History(score = Score(0, 0)), variant.Abalone)
    val situation  = Situation(board, P1)
    val validMoves = board.variant.validMoves(situation);

    "not generate a push when a same color marble prevents it (oooxo)" in {
      validMoves.get(Pos.E5).get.map(x => x.toString) should not contain "p1-Stone e5e2"
      validMoves.get(Pos.E5).get.map(x => x.toString) should not contain "p1-Stone e5e1"
    }

    "produce no move from a marble or a group or marbles that are stuck" in {
      validMoves.get(Pos.C1).get should beEmpty
      validMoves.get(Pos.D1).get should beEmpty
      validMoves.get(Pos.E1).get should beEmpty // (oooxo)
    }

    "not move more than 3 marbles" in {
      validMoves.get(Pos.E3).get.map(x => x.toString) should not contain "p1-Stone e3e7"
      validMoves.get(Pos.E3).get.map(x => x.toString) should not contain "p1-Stone e3e8"
      validMoves.get(Pos.E3).get.map(x => x.toString) should not contain "p1-Stone e3e9"
      validMoves.get(Pos.E3).get.map(x => x.toString) should not contain "p1-Stone e3d6"
      validMoves.get(Pos.E3).get.map(x => x.toString) should not contain "p1-Stone e3f7"
      validMoves.get(Pos.E3).get.size must_== 7
    }

    "find and compute all moves that push off (3v2, 3v1, 2v1)" in {
      validMoves.get(Pos.D8).get.map(x => x.toString) should contain("p1-Stone d8a5")
      validMoves.get(Pos.C7).get.map(x => x.toString) should contain("p1-Stone c7a5")
      validMoves.get(Pos.C7).get.map(x => x.toString) should contain("p1-Stone c7e9")
      validMoves.get(Pos.B6).get.map(x => x.toString) should contain("p1-Stone b6e9")

      validMoves.get(Pos.A1).get.map(x => x.toString) should contain("p1-Stone a1a4")
      validMoves.get(Pos.A1).get.map(x => x.toString) should not contain "p1-Stone a1a5"
    }

    "not create a move in case of 2v2 push" in {
      validMoves.get(Pos.A2).get.map(x => x.toString) should not contain "p1-Stone a2a4"
      validMoves.get(Pos.A2).get.map(x => x.toString) should not contain "p1-Stone a2a5"
    }

    "not create a move in case of 3v3 push" in {
      validMoves.get(Pos.E4).get.map(x => x.toString) should not contain "p1-Stone e4e7"
      validMoves.get(Pos.E4).get.map(x => x.toString) should not contain "p1-Stone e4e8"
      validMoves.get(Pos.E4).get.map(x => x.toString) should not contain "p1-Stone e4e9"
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
    val fenEndedGame = format.FEN("5/6/7/8/5SSSs/8/7/6/5 0 0 b 0 0")
    val board        = Board(fenEndedGame.pieces, History(score = Score(0, 0)), variant.Abalone)
    val situation    = Situation(board, P1)
    val game         = Game.apply(board.variant)
    val validMoves   = game.board.variant.validMoves(situation)
    val game2        = game.apply(validMoves(Pos.F5)(5))

    "increment the score of P1" in {
      board.history.score must_== Score(0, 0)
      game2.situation.board.history.score must_== Score(1, 0)
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
  "pushing out the only marble of P2 with a score of 5" should {
    val fenEndedGame = format.FEN("5/6/7/8/5SSSs/8/7/6/5 5 0 b 0 0")
    val board        = Board(fenEndedGame.pieces, History(score = Score(5, 0)), variant.Abalone)
    val situation    = Situation(board, P1)
    val game         = Game.apply(board.variant)
    val validMoves   = game.board.variant.validMoves(situation)
    val game2        = game.apply(validMoves(Pos.F5)(5))

    "increment the score of P1" in {
      board.history.score must_== Score(5, 0) // game.situation.board.history.score is Score(0,0)
      game2.situation.board.history.score must_== Score(6, 0)
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
  "P1 pushing out 6 marbles consecutively" should {
    val fenEndedGame = format.FEN("3ss/3Sss/4SS1/6S1/4SSSss/8/7/6/s4 0 0 b 0 0")
    val board        = Board(fenEndedGame.pieces, History(score = Score(0, 0)), variant.Abalone)
    val situation    = Situation(board, P1)
    val game         = Game.apply(board.variant)
    val validMoves   = game.board.variant.validMoves(situation)
    val game2        = game.apply(validMoves(Pos.E5)(5))     // e5h5
    val validMoves2  = game2.board.variant.validMoves(game2.situation)
    val game3        = game2.apply(validMoves2(Pos.A1)(0))   // a1a2
    val validMoves3  = game3.board.variant.validMoves(game3.situation)
    val game4        = game3.apply(validMoves3(Pos.F5)(5))   // f5i5
    val validMoves4  = game4.board.variant.validMoves(game4.situation)
    val game5        = game4.apply(validMoves4(Pos.A2)(3))   // a2a1
    val validMoves5  = game5.board.variant.validMoves(game5.situation)
    val game6        = game5.apply(validMoves5(Pos.H5)(5))   // h5h8
    val validMoves6  = game6.board.variant.validMoves(game6.situation)
    val game7        = game6.apply(validMoves6(Pos.A1)(0))   // a1a2
    val validMoves7  = game7.board.variant.validMoves(game7.situation)
    val game8        = game7.apply(validMoves7(Pos.H6)(4))   // h6h9
    val validMoves8  = game8.board.variant.validMoves(game8.situation)
    val game9        = game8.apply(validMoves8(Pos.A2)(3))   // a2a1
    val validMoves9  = game9.board.variant.validMoves(game9.situation)
    val game10       = game9.apply(validMoves9(Pos.G8)(3))   // g8i8
    val validMoves10 = game10.board.variant.validMoves(game10.situation)
    val game11       = game10.apply(validMoves10(Pos.A1)(0)) // a1a2
    val validMoves11 = game11.board.variant.validMoves(game11.situation)
    val game12       = game11.apply(validMoves11(Pos.G7)(4)) // g7i9

    "should enter a non reversible state only when pushing out" in {
      game8.board.variant.isIrreversible(validMoves8(Pos.A2)(3)) must_== false
      game9.board.variant.isIrreversible(validMoves9(Pos.G8)(3)) must_== true
      game10.board.variant.isIrreversible(validMoves10(Pos.A1)(0)) must_== false
    }

    "increment the score of P1 each time he plays a move" in {
      board.history.score must_== Score(0, 0)
      game2.situation.board.history.score must_== Score(1, 0)
      game4.situation.board.history.score must_== Score(2, 0)
      game6.situation.board.history.score must_== Score(3, 0)
      game8.situation.board.history.score must_== Score(4, 0)
      game10.situation.board.history.score must_== Score(5, 0)
      game12.situation.board.history.score must_== Score(6, 0)
    }

    "reset the halMovesSinceLastCapture after P1 push out" in {
      game2.situation.board.history.halfMoveClock must_== 0
      game3.situation.board.history.halfMoveClock must_== 1
      game4.situation.board.history.halfMoveClock must_== 0
      game5.situation.board.history.halfMoveClock must_== 1
      game6.situation.board.history.halfMoveClock must_== 0
      game8.situation.board.history.halfMoveClock must_== 0
      game10.situation.board.history.halfMoveClock must_== 0
      game12.situation.board.history.halfMoveClock must_== 0
    }

    "reduce the number of marbles on the board after P1 push out" in {
      board.pieces.size must_== 7 + 7
      game2.situation.board.pieces.size must_== 7 + 6
      game4.situation.board.pieces.size must_== 7 + 5
      game6.situation.board.pieces.size must_== 7 + 4
      game8.situation.board.pieces.size must_== 7 + 3
      game10.situation.board.pieces.size must_== 7 + 2
      game12.situation.board.pieces.size must_== 7 + 1
    }

    "trigger a win condition defined by the variant, and no stalemate is detected" in {
      game12.situation.end must_== true
      game12.situation.staleMate must_== false
      game12.situation.playable(true) must_== false
      game12.situation.status must_== Some(Status.VariantEnd)
      game12.situation.winner must_== Some(P1)
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
  "P2 and P1 moving left right left right several times" should {
    val fenEndedGame = format.FEN("3ss/3Sss/4SS1/6S1/4SSSss/8/7/6/s4 0 0 b 0 0")
    val board        = Board(fenEndedGame.pieces, History(score = Score(0, 0)), variant.Abalone)
    val situation    = Situation(board, P1)
    val game         = Game.apply(board.variant)
    val validMoves   = game.board.variant.validMoves(situation)
    val game2        =
      game.apply(validMoves(Pos.E5)(0)) // <- e5d5 0 is left in case there is a square on the left side
    val validMoves2 = game2.board.variant.validMoves(game2.situation)
    val game3       = game2.apply(validMoves2(Pos.A1)(2)) // -> a1b1
    val validMoves3 = game3.board.variant.validMoves(game3.situation)
    val game4       =
      game3.apply(validMoves3(Pos.D5)(3)) // -> d5e5 3 is right in case we can move left, upLeft, upRight
    val validMoves4 = game4.board.variant.validMoves(game4.situation)
    val game5       = game4.apply(validMoves4(Pos.B1)(0)) // <- b1a1
    val validMoves5 = game5.board.variant.validMoves(game5.situation)
    val game6       = game5.apply(validMoves5(Pos.E5)(0)) // <- e5d5
    val validMoves6 = game6.board.variant.validMoves(game6.situation)
    val game7       = game6.apply(validMoves6(Pos.A1)(2)) // -> a1b1
    val validMoves7 = game7.board.variant.validMoves(game7.situation)
    val game8       = game7.apply(validMoves7(Pos.D5)(3)) // -> d5e5
    val validMoves8 = game8.board.variant.validMoves(game8.situation)
    val game9       = game8.apply(validMoves8(Pos.B1)(0)) // <-  // DRAW !
    val validMoves9 = game9.board.variant.validMoves(game9.situation)
    val game10      = game9.apply(validMoves9(Pos.E5)(0)) // <- trying to play a neutral move

    "trigger a draw by repetition after seing the same situation for the 3rd time, and no stalemate is detected" in {
      // <situation> 1. e5d5, ..a1b1, 2. d5e5, ..b1a1, <situation> 3. e5d5, ..a1b1, 4. d5e5, ..b1a1 <situation>
      game8.situation.end must_== false
      game8.situation.playable(true) must_== true

      game9.situation.end must_== true
      game9.situation.staleMate must_== false
      game9.situation.playable(true) must_== false
      game9.situation.status must_== Some(Status.Draw)
      game9.situation.winner must_== None
    }

    "keep incrementing halMovesSinceLastCapture after each player move" in {
      game2.situation.board.history.halfMoveClock must_== 1
      game3.situation.board.history.halfMoveClock must_== 2
      game4.situation.board.history.halfMoveClock must_== 3
      game5.situation.board.history.halfMoveClock must_== 4
      game6.situation.board.history.halfMoveClock must_== 5
      game8.situation.board.history.halfMoveClock must_== 7
      game10.situation.board.history.halfMoveClock must_== 9
    }

    "keep being a draw even if in some weird case a neutral move could be played after" in {
      game10.situation.end must_== true
      game10.situation.playable(true) must_== false
      game10.situation.status must_== Some(Status.Draw)
      game10.situation.winner must_== None
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
  "P2 and P1 moving left right left right then something else but then come back to the same position again" should {
    val fenEndedGame = format.FEN("3ss/3Sss/4SS1/6S1/4SSSss/8/7/6/s4 0 0 b 0 0")
    val board        = Board(fenEndedGame.pieces, History(score = Score(0, 0)), variant.Abalone)
    val situation    = Situation(board, P1)
    val game         = Game.apply(board.variant)
    val validMoves   = game.board.variant.validMoves(situation)
    val game2        =
      game.apply(validMoves(Pos.E5)(0)) // <- e5d5 0 is left in case there is a square on the left side
    val validMoves2 = game2.board.variant.validMoves(game2.situation)
    val game3       = game2.apply(
      validMoves2(Pos.A1)(2)
    ) // -> a1b1 moving right with index 2 because we can not move left (else, would be 3)
    val validMoves3 = game3.board.variant.validMoves(game3.situation)
    val game4       =
      game3.apply(validMoves3(Pos.D5)(3)) // -> d5e5 3 is right in case we can move left, upLeft, upRight
    val validMoves4  = game4.board.variant.validMoves(game4.situation)
    val game5        = game4.apply(validMoves4(Pos.B1)(0)) // <- b1a1
    val validMoves5  = game5.board.variant.validMoves(game5.situation)
    val game6        = game5.apply(validMoves5(Pos.E5)(0)) // <- e5d5
    val validMoves6  = game6.board.variant.validMoves(game6.situation)
    val game7        = game6.apply(validMoves6(Pos.A1)(2)) // -> a1b1
    val validMoves7  = game7.board.variant.validMoves(game7.situation)
    val game8        = game7.apply(validMoves7(Pos.D5)(3)) // -> d5e5
    val validMoves8  = game8.board.variant.validMoves(game8.situation)
    val game9        = game8.apply(validMoves8(Pos.B1)(1)) // upLeft
    val validMoves9  = game9.board.variant.validMoves(game9.situation)
    val game10       = game9.apply(validMoves9(Pos.E5)(0)) // <- e5d5
    val validMoves10 = game10.board.variant.validMoves(game10.situation)
    val game11       = game10.apply(
      validMoves10(Pos.B2)(5)
    ) // downLeft to reach A1 -> // not a draw anymore, even though we saw the situation 2 times before
    val validMoves11 = game11.board.variant.validMoves(game11.situation)
    val game12       = game11.apply(validMoves11(Pos.D5)(3)) // -> d5e5

    "not trigger a draw anymore" in {
      game10.situation.end must_== false
      game10.situation.playable(true) must_== true
      game10.situation.status must_== None
      game10.situation.winner must_== None

      game11.situation.end must_== false
      game11.situation.playable(true) must_== true
      game11.situation.status must_== None
      game11.situation.winner must_== None

      game12.situation.end must_== false
      game12.situation.playable(true) must_== true
      game12.situation.status must_== None
      game12.situation.winner must_== None
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
  "P2 and P1 moving left right left right then something else but then come back to the same position again, and then do a 3fold repetition" should {
    val fenEndedGame = format.FEN("3ss/3Sss/4SS1/6S1/4SSSss/8/7/6/s4 0 0 b 0 0")
    val board        = Board(fenEndedGame.pieces, History(score = Score(0, 0)), variant.Abalone)
    val situation    = Situation(board, P1)
    val game         = Game.apply(board.variant)
    val validMoves   = game.board.variant.validMoves(situation)
    val game2        =
      game.apply(validMoves(Pos.E5)(0)) // <- e5d5 0 is left in case there is a square on the left side
    val validMoves2 = game2.board.variant.validMoves(game2.situation)
    val game3       = game2.apply(
      validMoves2(Pos.A1)(2)
    ) // -> a1b1 moving right with index 2 because we can not move left (else, would be 3)
    val validMoves3 = game3.board.variant.validMoves(game3.situation)
    val game4       =
      game3.apply(validMoves3(Pos.D5)(3)) // -> d5e5 3 is right in case we can move left, upLeft, upRight
    val validMoves4  = game4.board.variant.validMoves(game4.situation)
    val game5        = game4.apply(validMoves4(Pos.B1)(0)) // <- b1a1
    val validMoves5  = game5.board.variant.validMoves(game5.situation)
    val game6        = game5.apply(validMoves5(Pos.E5)(0)) // <- e5d5
    val validMoves6  = game6.board.variant.validMoves(game6.situation)
    val game7        = game6.apply(validMoves6(Pos.A1)(2)) // -> a1b1
    val validMoves7  = game7.board.variant.validMoves(game7.situation)
    val game8        = game7.apply(validMoves7(Pos.D5)(3)) // -> d5e5
    val validMoves8  = game8.board.variant.validMoves(game8.situation)
    val game9        = game8.apply(validMoves8(Pos.B1)(1)) // upLeft
    val validMoves9  = game9.board.variant.validMoves(game9.situation)
    val game10       = game9.apply(validMoves9(Pos.E5)(0)) // <- e5d5
    val validMoves10 = game10.board.variant.validMoves(game10.situation)
    val game11       = game10.apply(
      validMoves10(Pos.B2)(5)
    ) // downLeft to reach A1 -> // not a draw anymore, even though we saw the situation 2 times before
    val validMoves11 = game11.board.variant.validMoves(game11.situation)
    val game12       = game11.apply(validMoves11(Pos.D5)(3)) // -> d5e5
    val validMoves12 = game12.board.variant.validMoves(game12.situation)
    val game13       = game12.apply(validMoves12(Pos.A1)(1)) // upRight to B2
    val validMoves13 = game13.board.variant.validMoves(game13.situation)
    val game14       = game13.apply(validMoves13(Pos.E5)(0)) // <- e5d5
    val validMoves14 = game14.board.variant.validMoves(game14.situation)
    val game15       = game14.apply(validMoves14(Pos.B2)(5)) // downLeft to reach A1
    val validMoves15 = game15.board.variant.validMoves(game15.situation)
    val game16       = game15.apply(validMoves15(Pos.D5)(3)) // -> d5e5
    val validMoves16 = game16.board.variant.validMoves(game16.situation)
    val game17       = game16.apply(validMoves16(Pos.A1)(1)) // upRight to B2 : draw

    "trigger a draw only from AFTER the 3fold repetition sequence" in {
      game16.situation.end must_== false
      game16.situation.playable(true) must_== true
      game16.situation.status must_== None
      game16.situation.winner must_== None

      game17.situation.end must_== true
      game17.situation.playable(true) must_== false
      game17.situation.status must_== Some(Status.Draw)
      game17.situation.winner must_== None
    }
  }
}

class AbaloneVariantTestIsometry extends strategygames.chess.ChessTest {

  val abaloneGameActionStrs = Vector(
    Vector("a1d4"),
    Vector("e9e6"),
    Vector("b1d3"),
    Vector("e1e4"),
    Vector("c2e4"),
    Vector("d8d6"),
    Vector("b3e3"),
    Vector("g3f4"),
    Vector("i9f6"),
    Vector("e8e5"),
    Vector("a2c2"),
    Vector("f2c2"),
    Vector("h9f7"),
    Vector("e2b2"),
    Vector("a2b3"),
    Vector("e7e4"),
    Vector("i8g6"),
    Vector("e6e3"),
    Vector("b3e3"),
    Vector("d7e6"),
    Vector("h8h6"),
    Vector("g4d4")
  )

  "Test Every move can be loaded from fen" in {
    val gameFamily   = Abalone.gameFamily
    val lib          = gameFamily.gameLogic
    val stratVariant = StratVariant(lib, Abalone.key).get

    _testEveryMoveLoadFenIsometry(lib, StratFen(lib, Abalone.initialFen.value), stratVariant)(
      abaloneGameActionStrs.flatten.toList.map(uciStr => StratUci(lib, gameFamily, uciStr).get)
    ) must beValid.like(gameData => {
      val fen1 = StratForsyth.>>(lib, gameData.game)
      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
      fen1 must_== fen2
    })
  }

}
