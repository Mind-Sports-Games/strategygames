package strategygames.abalone

import org.specs2.matcher.ValidatedMatchers

import strategygames.Score
//import strategygames.{ Score, Status }
//import strategygames.format.{ FEN => StratFen, Forsyth => StratForsyth, Uci => StratUci }
//import strategygames.variant.{ Variant => StratVariant }
import variant.Abalone

class AbaloneVariantTest extends AbaloneTest with ValidatedMatchers {
  /*
   *      · · · · ·
   *     · 2 · · · ·
   *    · · 1 · · · ·
   *   · · · 1 · · · ·
   *  · · · 2 1 · · · ·
   *   · · · · · · · ·
   *    · · · · · · ·
   *     · · · · · ·
   *      · · · · ·
   *  P1: E5, F5, G5
   *  P2: E4, H5
   */
  "custom basic position" should {
    val fen        = format.FEN("5/6/7/4s3/4SSSs1/8/4s3/7/6/5 0 0 b 0 0")
    val board      = Board(fen.pieces(Abalone.boardType), History(), Abalone)
    val situation  = Situation(board, P1)
    val validMoves = board.variant.validMoves(situation)
    val moves      = board.variant.validMoves(situation).flatMap(_._2)
    val movesOf1   = moves.filter(m => board.variant.boardType.norm(m.dest - m.orig) == 1)
    val movesOf23  = moves.filter(m => board.variant.boardType.norm(m.dest - m.orig) > 1)
    val game       = Game(board.variant) // @TODO : check why it does not set the piecemap correctly. See a few lines below we have to use board.pieces instead.
    val game2       = game(validMoves(new Pos(4, 6)).find(m => m.dest == new Pos(5, 4)).get)// g5e6
    val validMoves2 = game2.board.variant.validMoves(game2.situation)
    val game3       = game2(validMoves2(new Pos(4, 7)).find(m => m.dest == new Pos(3, 6)).get)// h5g4
    //val validMoves3 = game3.board.variant.validMoves(game3.situation)
    val game4       = game3(validMoves2(new Pos(5, 4)).find(m => m.dest == new Pos(4, 6)).get)// e6g5
    //val validMoves4 = game4.board.variant.validMoves(game4.situation)
    val game5       = game4(validMoves2(new Pos(3, 6)).find(m => m.dest == new Pos(4, 7)).get)// g4h5
    //val validMoves5 = game5.board.variant.validMoves(game5.situation)
    val game6       = game5(validMoves2(new Pos(4, 5)).find(m => m.dest == new Pos(3, 6)).get)// f5g4
    val validMoves6 = game6.board.variant.validMoves(game6.situation)

    "compute the correct number of moves of 1 marble" in {
      movesOf1.size must_== 11
    }

    "compute the correct number of moves of 2 and 3 marbles" in {
      movesOf23.size must_== (1 + 1 + 5) + (1 + 1 + 2)
    }

    "not have an intersection between moves of 1 marble and other moves" in {
      moves.size must_== movesOf1.size + movesOf23.size
    }

    "compute the right set of moves of 2 and 3 marbles" in {
      movesOf23.map(_.toUci.toString) should contain("Move(e5,h5)")
      movesOf23.map(_.toUci.toString) should contain("Move(e5,g6)")
      movesOf23.map(_.toUci.toString) should contain("Move(e5,h6)")

      movesOf23.map(_.toUci.toString) should contain("Move(f5,d5)")
      movesOf23.map(_.toUci.toString) should contain("Move(f5,e6)")
      movesOf23.map(_.toUci.toString) should contain("Move(f5,h5)")
      movesOf23.map(_.toUci.toString) should contain("Move(f5,h6)")
      movesOf23.map(_.toUci.toString) should contain("Move(f5,g4)")

      movesOf23.map(_.toUci.toString) should contain("Move(g5,d5)")
      movesOf23.map(_.toUci.toString) should contain("Move(g5,e6)")
      movesOf23.map(_.toUci.toString) should contain("Move(g5,f6)")
    }

    "side moves of 3 do move 3 marbles" in {
      game2.situation.board.pieces.contains(new Pos(5, 4)) must_== true
      game2.situation.board.pieces.contains(new Pos(5, 5)) must_== true
      game2.situation.board.pieces.contains(new Pos(5, 6)) must_== true
      game2.situation.board.pieces.contains(new Pos(3, 4)) must_== true
      game2.situation.board.pieces.contains(new Pos(4, 7)) must_== true
      game2.situation.board.pieces.size must_== 5
      validMoves2(new Pos(3, 4)).map(_.toUci.toString) should contain("Move(e4,e5)")
      validMoves2(new Pos(3, 4)).map(_.toUci.toString) should contain("Move(e4,f5)")
      validMoves2(new Pos(4, 7)).map(_.toUci.toString) should contain("Move(h5,g5)")

      board.pieces must_== game5.situation.board.pieces
      game5.situation.board.pieces.size must_== 5
    }

    "side moves of 2 do move 2 marbles" in {
      game6.situation.board.pieces.contains(new Pos(4, 4)) must_== true
      game6.situation.board.pieces.contains(new Pos(3, 5)) must_== true
      game6.situation.board.pieces.contains(new Pos(3, 6)) must_== true
      game6.situation.board.pieces.contains(new Pos(3, 4)) must_== true
      game6.situation.board.pieces.contains(new Pos(4, 7)) must_== true
      game6.situation.board.pieces.size must_== 5
      validMoves6(new Pos(3, 4)).map(_.toUci.toString) should not contain "Move(e4,f4)"
      validMoves6(new Pos(3, 4)).map(_.toUci.toString) should not contain "Move(e4,e5)"
      validMoves6(new Pos(3, 4)).map(_.toUci.toString) should contain("Move(e4,f5)")
      validMoves6(new Pos(4, 7)).map(_.toUci.toString) should contain("Move(h5,g5)")
    }
  }

  /*
   *     2 2 · 1 1
   *    2 2 2 1 1 1
   *   · 2 2 · 1 1 ·
   *  · · · · · · · ·
   * · · · · · · · · ·
   *  · · · · · · · ·
   *   · 1 1 · 2 2 ·
   *    1 1 1 2 2 2
   *     1 1 · 2 2
   */
  "\"Belgian daisy\" start position" should {
    val fen        = Abalone.initialFen
    val board      = Board(fen.pieces(Abalone.boardType), History(), Abalone)
    val situation  = Situation(board, P1)
    val validMoves = board.variant.validMoves(situation)
    val moves      = validMoves.flatMap(_._2)
    val lineMoves  = board.variant.validMoves_line(situation).flatMap(_._2)
    val jumpMoves  = board.variant.validMoves_jump(situation).flatMap(_._2)
    val movesOf1   = moves.filter(m => board.variant.boardType.norm(m.dest - m.orig) == 1)
    val movesOf2   = moves.filter(m => board.variant.boardType.norm(m.dest - m.orig) == 2)
    val movesOf3   = moves.filter(m => board.variant.boardType.norm(m.dest - m.orig) > 2)

    "compute the correct number of moves" in {
      moves.size must_== 52

      lineMoves.size must_== 44
      jumpMoves.size must_== 8

      movesOf1.size must_== 20
      movesOf2.size must_== 28
      movesOf3.size must_== 4

      val of23: Move => Boolean = m => board.variant.boardType.norm(m.dest - m.orig) > 1

      validMoves.get(new Pos(0, 0)).get.filter(of23).size must_== 3
      validMoves.get(new Pos(1, 0)).get.filter(of23).size must_== 2
      validMoves.get(new Pos(0, 1)).get.filter(of23).size must_== 2
      validMoves.get(new Pos(1, 1)).get.filter(of23).size must_== 2
      validMoves.get(new Pos(1, 2)).get.filter(of23).size must_== 2
      validMoves.get(new Pos(2, 1)).get.filter(of23).size must_== 2
      validMoves.get(new Pos(2, 2)).get.filter(of23).size must_== 3
    }
  }

  "\"Snakes variant\" start position" should {
    val fen        = format.FEN("sssss/5s/6s/1SSSSS1s/1S5s1/S1sssss1/S6/S5/SSSSS 0 0 b 0 0")
    val board      = Board(fen.pieces(Abalone.boardType), History(), Abalone)
    val situation  = Situation(board, P1)
    val validMoves = board.variant.validMoves(situation)
    val moves      = validMoves.flatMap(_._2)
    val lineMoves  = board.variant.validMoves_line(situation).flatMap(_._2)
    val jumpMoves  = board.variant.validMoves_jump(situation).flatMap(_._2)
    val movesOf1   = moves.filter(m => board.variant.boardType.norm(m.dest - m.orig) == 1)
    val movesOf2   = moves.filter(m => board.variant.boardType.norm(m.dest - m.orig) == 2)
    val movesOf3   = moves.filter(m => board.variant.boardType.norm(m.dest - m.orig) > 2)

    "compute the correct number of moves" in {
      moves.size must_== 98

      lineMoves.size must_== 48
      jumpMoves.size must_== 50

      movesOf1.size must_== 40
      movesOf2.size must_== 35
      movesOf3.size must_== 23
    }
  }

  "\"Alien\" start position" should {
    val fen       = format.FEN("s1s1s/1sSSs1/1sSsSs1/3ss3/9/3SS3/1SsSsS1/1SssS1/S1S1S 0 0 b 0 0")
    val board     = Board(fen.pieces(Abalone.boardType), History(), Abalone)
    val situation = Situation(board, P1)
    val validMoves = board.variant.validMoves(situation)
    val moves      = validMoves.flatMap(_._2)
    val lineMoves  = board.variant.validMoves_line(situation).flatMap(_._2)
    val jumpMoves  = board.variant.validMoves_jump(situation).flatMap(_._2)
    val movesOf1   = moves.filter(m => board.variant.boardType.norm(m.dest - m.orig) == 1)
    val movesOf2   = moves.filter(m => board.variant.boardType.norm(m.dest - m.orig) == 2)
    val movesOf3   = moves.filter(m => board.variant.boardType.norm(m.dest - m.orig) > 2)

    "compute the correct number of moves" in {
      moves.size must_== 48

      lineMoves.size must_== 44
      jumpMoves.size must_== 4

      movesOf1.size must_== 28
      movesOf2.size must_== 20
      movesOf3.size must_== 0
    }
  }

  "\"Domination\" start position" should {
    val fen       = format.FEN("5/S4s/SS3ss/SSS1ssss/3S1S3/ssss1SSS/ss3SS/s4S/5 0 0 b 0 0")
    val board     = Board(fen.pieces(Abalone.boardType), History(), Abalone)
    val situation = Situation(board, P1)
    val validMoves = board.variant.validMoves(situation)
    val moves      = validMoves.flatMap(_._2)
    val lineMoves  = board.variant.validMoves_line(situation).flatMap(_._2)
    val jumpMoves  = board.variant.validMoves_jump(situation).flatMap(_._2)
    val movesOf1   = moves.filter(m => board.variant.boardType.norm(m.dest - m.orig) == 1)
    val movesOf2   = moves.filter(m => board.variant.boardType.norm(m.dest - m.orig) == 2)
    val movesOf3   = moves.filter(m => board.variant.boardType.norm(m.dest - m.orig) > 2)

    "compute the correct number of moves" in {
      moves.size must_== 80

      lineMoves.size must_== 52
      jumpMoves.size must_== 28

      movesOf1.size must_== 28
      movesOf2.size must_== 34
      movesOf3.size must_== 18
    }

    "find the moves pushing marbles" in {
      val of23: Move => Boolean = m => board.variant.boardType.norm(m.dest - m.orig) > 1

      validMoves.get(new Pos(2, 1)).get.filter(of23).map(_.toUci.toString) should contain("Move(b3,e6)")
      validMoves.get(new Pos(3, 2)).get.filter(of23).map(_.toUci.toString) should contain("Move(c4,e6)")
      validMoves.get(new Pos(6, 7)).get.filter(of23).map(_.toUci.toString) should contain("Move(h7,e4)")
      validMoves.get(new Pos(5, 6)).get.filter(of23).map(_.toUci.toString) should contain("Move(g6,e4)")
    }
  }

  "custom start position with a total of 29 marbles" should {
    val fen       = format.FEN("1s3/2sSss/3s2S/ssSS2S1/4SSS2/8/7/3Ss1/2s1s 5 5 b 0 0")
    val board     = Board(fen.pieces(Abalone.boardType), History(score = Score(5, 5)), Abalone)
    var situation = Situation(board, P1)
    var validMoves = board.variant.validMoves(situation)
    val moves      = validMoves.flatMap(_._2)
    var lineMoves  = board.variant.validMoves_line(situation).flatMap(_._2)
    var jumpMoves  = board.variant.validMoves_jump(situation).flatMap(_._2)
    var movesOf1   = moves.filter(m => board.variant.boardType.norm(m.dest - m.orig) == 1)
    var movesOf2   = moves.filter(m => board.variant.boardType.norm(m.dest - m.orig) == 2)
    var movesOf3   = moves.filter(m => board.variant.boardType.norm(m.dest - m.orig) > 2)

    "compute the correct number of moves" in {
      moves.size must_== 58

      lineMoves.size must_== 41
      jumpMoves.size must_== 17

      movesOf1.size must_== 32
      movesOf2.size must_== 20
      movesOf3.size must_== 6
    }

    situation    = Situation(board, P2)
    validMoves   = board.variant.validMoves(situation)
    val moves_p2 = validMoves.flatMap(_._2)
    lineMoves    = board.variant.validMoves_line(situation).flatMap(_._2)
    jumpMoves    = board.variant.validMoves_jump(situation).flatMap(_._2)
    movesOf1     = moves_p2.filter(m => board.variant.boardType.norm(m.dest - m.orig) == 1)
    movesOf2     = moves_p2.filter(m => board.variant.boardType.norm(m.dest - m.orig) == 2)
    movesOf3     = moves_p2.filter(m => board.variant.boardType.norm(m.dest - m.orig) > 2)

    "compute the correct number of moves for P2" in {
      moves_p2.size must_== 44

      lineMoves.size must_== 34
      jumpMoves.size must_== 10

      movesOf1.size must_== 31
      movesOf2.size must_== 11
      movesOf3.size must_== 2
    }
  }

  //TODO
//  "custom start position" should {
//    val fen       = format.FEN("SSS2/2sSSS/1SSssss/3S4/3S5/2S5/1s5/ssss2/SSSSs 5 5 b 0 0")
//    val board     = Board(fen.pieces, History(score = Score(0, 0)), Abalone)
//    val situation = Situation(board, P1)
//    val movesOf1  = board.variant.validMovesOf1(situation)
//    val movesOf23 = board.variant.validMovesOf2And3(situation)
//
//    "compute 32 moves of 1 marble" in {
//      movesOf1.foldLeft(0)(_ + _._2.size) must_== 2 + 1 + 1 + 1 + 2 + 1 + 5 + 2 + 3 + 4 + 4 + 1
//    }
//
//    "compute the correct number of moves of 2 and 3 marbles" in {
//      movesOf23.foldLeft(0)(_ + _._2.size) must_== (3 + 4 + (1 + 1 + 1 + 2 + 2 + 3 + 2)) + (3 + 1 + 3)
//    }
//
//    "not have an intersection between moves of 1 marble and other moves" in {
//      board.variant.validMoves(situation).foldLeft(0)(_ + _._2.size) must_== movesOf1.foldLeft(0)(
//        _ + _._2.size
//      ) + movesOf23.foldLeft(0)(_ + _._2.size)
//    }
//
//    "not generate a move for a marble that is blocked" in {
//      movesOf1.get(Pos.A1).get should beEmpty
//      movesOf23.get(Pos.A1).get should beEmpty
//    }
//
//    "find and compute all moves that push off (3v2, 3v1, 2v1)" in {
//      movesOf23.get(Pos.B1).get.map(x => x.toUci.toString) should contain("Move(b1,e1)")
//      movesOf23.get(Pos.C1).get.map(x => x.toUci.toString) should contain("Move(c1,e1)")
//      movesOf23.get(Pos.E6).get.map(x => x.toUci.toString) should contain("Move(e6,b3)")
//    }
//  }
//
//  "special position to test some edge cases" should {
//    val fen        = format.FEN("s4/Ss4/S1s4/S2S4/s3S4/s3S3/S3S2/Ssssss/SsSSS 5 5 b 0 0")
//    val board      = Board(fen.pieces, History(score = Score(0, 0)), Abalone)
//    val situation  = Situation(board, P1)
//    val validMoves = board.variant.validMoves(situation);
//
//    "not generate a push when a same color marble prevents it (oooxo)" in {
//      validMoves.get(Pos.E5).get.map(x => x.toString) should not contain "p1-Stone e5e2"
//      validMoves.get(Pos.E5).get.map(x => x.toString) should not contain "p1-Stone e5e1"
//    }
//
//    "produce no move from a marble or a group or marbles that are stuck" in {
//      validMoves.get(Pos.C1).get should beEmpty
//      validMoves.get(Pos.D1).get should beEmpty
//      validMoves.get(Pos.E1).get should beEmpty // (oooxo)
//    }
//
//    "not move more than 3 marbles" in {
//      validMoves.get(Pos.E3).get.map(x => x.toString) should not contain "p1-Stone e3e7"
//      validMoves.get(Pos.E3).get.map(x => x.toString) should not contain "p1-Stone e3e8"
//      validMoves.get(Pos.E3).get.map(x => x.toString) should not contain "p1-Stone e3e9"
//      validMoves.get(Pos.E3).get.map(x => x.toString) should not contain "p1-Stone e3d6"
//      validMoves.get(Pos.E3).get.map(x => x.toString) should not contain "p1-Stone e3f7"
//      validMoves.get(Pos.E3).get.size must_== 7
//    }
//
//    "find and compute all moves that push off (3v2, 3v1, 2v1)" in {
//      validMoves.get(Pos.D8).get.map(x => x.toString) should contain("p1-Stone d8a5")
//      validMoves.get(Pos.C7).get.map(x => x.toString) should contain("p1-Stone c7a5")
//      validMoves.get(Pos.C7).get.map(x => x.toString) should contain("p1-Stone c7e9")
//      validMoves.get(Pos.B6).get.map(x => x.toString) should contain("p1-Stone b6e9")
//
//      validMoves.get(Pos.A1).get.map(x => x.toString) should contain("p1-Stone a1a4")
//      validMoves.get(Pos.A1).get.map(x => x.toString) should not contain "p1-Stone a1a5"
//    }
//
//    "not create a move in case of 2v2 push" in {
//      validMoves.get(Pos.A2).get.map(x => x.toString) should not contain "p1-Stone a2a4"
//      validMoves.get(Pos.A2).get.map(x => x.toString) should not contain "p1-Stone a2a5"
//    }
//
//    "not create a move in case of 3v3 push" in {
//      validMoves.get(Pos.E4).get.map(x => x.toString) should not contain "p1-Stone e4e7"
//      validMoves.get(Pos.E4).get.map(x => x.toString) should not contain "p1-Stone e4e8"
//      validMoves.get(Pos.E4).get.map(x => x.toString) should not contain "p1-Stone e4e9"
//    }
//  }
//
//  /*
//   *      _ _ _ _ _
//   *     _ _ _ _ _ _
//   *    _ _ _ _ _ _ _
//   *   _ _ _ _ _ _ _ _
//   *  _ _ _ _ _ 0 0 0 *
//   *   _ _ _ _ _ _ _ _
//   *    _ _ _ _ _ _ _
//   *     _ _ _ _ _ _
//   *      _ _ _ _ _
//   */
//  "pushing out the only marble of P2 with a score of 0" should {
//    val fenEndedGame = format.FEN("5/6/7/8/5SSSs/8/7/6/5 0 0 b 0 0")
//    val board        = Board(fenEndedGame.pieces, History(score = Score(0, 0)), Abalone)
//    val situation    = Situation(board, P1)
//    val game         = Game.apply(board.variant)
//    val validMoves   = game.board.variant.validMoves(situation)
//    val game2        = game.apply(validMoves(Pos.F5)(5))
//
//    "increment the score of P1" in {
//      board.history.score must_== Score(0, 0)
//      game2.situation.board.history.score must_== Score(1, 0)
//    }
//
//    "trigger a stalemate" in {
//      game2.situation.end must_== true
//      game2.situation.stalemate must_== true
//      game2.situation.playable(true) must_== false
//      game2.situation.status must_== Some(Status.Stalemate)
//      game2.situation.winner must_== None
//    }
//  }
//
//  /*
//   *     _ _ _ _ _
//   *    _ _ _ _ _ _
//   *   _ _ _ _ _ _ _
//   *  _ _ _ _ _ _ _ _
//   * _ _ _ _ _ 0 0 0 *   5 - 0
//   *  _ _ _ _ _ _ _ _
//   *   _ _ _ _ _ _ _
//   *    _ _ _ _ _ _
//   *     _ _ _ _ _
//   */
//  "pushing out the only marble of P2 with a score of 5" should {
//    val fenEndedGame = format.FEN("5/6/7/8/5SSSs/8/7/6/5 5 0 b 0 0")
//    val board        = Board(fenEndedGame.pieces, History(score = Score(5, 0)), Abalone)
//    val situation    = Situation(board, P1)
//    val game         = Game.apply(board.variant)
//    val validMoves   = game.board.variant.validMoves(situation)
//    val game2        = game.apply(validMoves(Pos.F5)(5))
//
//    "increment the score of P1" in {
//      board.history.score must_== Score(5, 0) // game.situation.board.history.score is Score(0,0)
//      game2.situation.board.history.score must_== Score(6, 0)
//    }
//
//    "trigger a win condition defined by the variant, even though it detects the stalemate" in {
//      game2.situation.end must_== true
//      game2.situation.stalemate must_== true
//      game2.situation.playable(true) must_== false
//      game2.situation.status must_== Some(Status.VariantEnd)
//      game2.situation.winner must_== Some(P1)
//    }
//  }
//
//  /*
//   *      _ _ _ * *
//   *     _ _ _ 0 * *
//   *    _ _ _ _ 0 0 _
//   *   _ _ _ _ _ _ 0 _
//   *  _ _ _ _ 0 0 0 * *
//   *   _ _ _ _ _ _ _ _
//   *    _ _ _ _ _ _ _
//   *     _ _ _ _ _ _
//   *      _ _ _ _
//   */
//  "P1 pushing out 6 marbles consecutively" should {
//    val fenEndedGame = format.FEN("3ss/3Sss/4SS1/6S1/4SSSss/8/7/6/s4 0 0 b 0 0")
//    val board        = Board(fenEndedGame.pieces, History(score = Score(0, 0)), Abalone)
//    val situation    = Situation(board, P1)
//    val game         = Game.apply(board.variant)
//    val validMoves   = game.board.variant.validMoves(situation)
//    val game2        = game.apply(validMoves(Pos.E5)(5))     // e5h5
//    val validMoves2  = game2.board.variant.validMoves(game2.situation)
//    val game3        = game2.apply(validMoves2(Pos.A1)(0))   // a1a2
//    val validMoves3  = game3.board.variant.validMoves(game3.situation)
//    val game4        = game3.apply(validMoves3(Pos.F5)(5))   // f5i5
//    val validMoves4  = game4.board.variant.validMoves(game4.situation)
//    val game5        = game4.apply(validMoves4(Pos.A2)(3))   // a2a1
//    val validMoves5  = game5.board.variant.validMoves(game5.situation)
//    val game6        = game5.apply(validMoves5(Pos.H5)(5))   // h5h8
//    val validMoves6  = game6.board.variant.validMoves(game6.situation)
//    val game7        = game6.apply(validMoves6(Pos.A1)(0))   // a1a2
//    val validMoves7  = game7.board.variant.validMoves(game7.situation)
//    val game8        = game7.apply(validMoves7(Pos.H6)(4))   // h6h9
//    val validMoves8  = game8.board.variant.validMoves(game8.situation)
//    val game9        = game8.apply(validMoves8(Pos.A2)(3))   // a2a1
//    val validMoves9  = game9.board.variant.validMoves(game9.situation)
//    val game10       = game9.apply(validMoves9(Pos.G8)(3))   // g8i8
//    val validMoves10 = game10.board.variant.validMoves(game10.situation)
//    val game11       = game10.apply(validMoves10(Pos.A1)(0)) // a1a2
//    val validMoves11 = game11.board.variant.validMoves(game11.situation)
//    val game12       = game11.apply(validMoves11(Pos.G7)(4)) // g7i9
//
//    "should enter a non reversible state only when pushing out" in {
//      game8.board.variant.isIrreversible(validMoves8(Pos.A2)(3)) must_== false
//      game9.board.variant.isIrreversible(validMoves9(Pos.G8)(3)) must_== true
//      game10.board.variant.isIrreversible(validMoves10(Pos.A1)(0)) must_== false
//    }
//
//    "increment the score of P1 each time he plays a move" in {
//      board.history.score must_== Score(0, 0)
//      game2.situation.board.history.score must_== Score(1, 0)
//      game4.situation.board.history.score must_== Score(2, 0)
//      game6.situation.board.history.score must_== Score(3, 0)
//      game8.situation.board.history.score must_== Score(4, 0)
//      game10.situation.board.history.score must_== Score(5, 0)
//      game12.situation.board.history.score must_== Score(6, 0)
//    }
//
//    "reset the halMovesSinceLastCapture after P1 push out" in {
//      game2.situation.board.history.halfMoveClock must_== 0
//      game3.situation.board.history.halfMoveClock must_== 1
//      game4.situation.board.history.halfMoveClock must_== 0
//      game5.situation.board.history.halfMoveClock must_== 1
//      game6.situation.board.history.halfMoveClock must_== 0
//      game8.situation.board.history.halfMoveClock must_== 0
//      game10.situation.board.history.halfMoveClock must_== 0
//      game12.situation.board.history.halfMoveClock must_== 0
//    }
//
//    "reduce the number of marbles on the board after P1 push out" in {
//      board.pieces.size must_== 7 + 7
//      game2.situation.board.pieces.size must_== 7 + 6
//      game4.situation.board.pieces.size must_== 7 + 5
//      game6.situation.board.pieces.size must_== 7 + 4
//      game8.situation.board.pieces.size must_== 7 + 3
//      game10.situation.board.pieces.size must_== 7 + 2
//      game12.situation.board.pieces.size must_== 7 + 1
//    }
//
//    "trigger a win condition defined by the variant, and no stalemate is detected" in {
//      game12.situation.end must_== true
//      game12.situation.stalemate must_== false
//      game12.situation.playable(true) must_== false
//      game12.situation.status must_== Some(Status.VariantEnd)
//      game12.situation.winner must_== Some(P1)
//    }
//  }
//
//  /*
//   *      _ _ _ * *
//   *     _ _ _ 0 * *
//   *    _ _ _ _ 0 0 _
//   *   _ _ _ _ _ _ 0 _
//   *  _ _ _ _ 0 0 0 * *
//   *   _ _ _ _ _ _ _ _
//   *    _ _ _ _ _ _ _
//   *     _ _ _ _ _ _
//   *      _ _ _ _ _
//   */
//  "P2 and P1 moving left right left right several times" should {
//    val fenEndedGame = format.FEN("3ss/3Sss/4SS1/6S1/4SSSss/8/7/6/s4 0 0 b 0 0")
//    val board        = Board(fenEndedGame.pieces, History(score = Score(0, 0)), Abalone)
//    val situation    = Situation(board, P1)
//    val game         = Game.apply(board.variant)
//    val validMoves   = game.board.variant.validMoves(situation)
//    val game2        =
//      game.apply(validMoves(Pos.E5)(0)) // <- e5d5 0 is left in case there is a square on the left side
//    val validMoves2 = game2.board.variant.validMoves(game2.situation)
//    val game3       = game2.apply(validMoves2(Pos.A1)(2)) // -> a1b1
//    val validMoves3 = game3.board.variant.validMoves(game3.situation)
//    val game4       =
//      game3.apply(validMoves3(Pos.D5)(3)) // -> d5e5 3 is right in case we can move left, upLeft, upRight
//    val validMoves4 = game4.board.variant.validMoves(game4.situation)
//    val game5       = game4.apply(validMoves4(Pos.B1)(0)) // <- b1a1
//    val validMoves5 = game5.board.variant.validMoves(game5.situation)
//    val game6       = game5.apply(validMoves5(Pos.E5)(0)) // <- e5d5
//    val validMoves6 = game6.board.variant.validMoves(game6.situation)
//    val game7       = game6.apply(validMoves6(Pos.A1)(2)) // -> a1b1
//    val validMoves7 = game7.board.variant.validMoves(game7.situation)
//    val game8       = game7.apply(validMoves7(Pos.D5)(3)) // -> d5e5
//    val validMoves8 = game8.board.variant.validMoves(game8.situation)
//    val game9       = game8.apply(validMoves8(Pos.B1)(0)) // <-  // DRAW !
//    val validMoves9 = game9.board.variant.validMoves(game9.situation)
//    val game10      = game9.apply(validMoves9(Pos.E5)(0)) // <- trying to play a neutral move
//
//    "trigger a draw by repetition after seing the same situation for the 3rd time, and no stalemate is detected" in {
//      // <situation> 1. e5d5, ..a1b1, 2. d5e5, ..b1a1, <situation> 3. e5d5, ..a1b1, 4. d5e5, ..b1a1 <situation>
//      game8.situation.end must_== false
//      game8.situation.playable(true) must_== true
//
//      game9.situation.end must_== true
//      game9.situation.stalemate must_== false
//      game9.situation.playable(true) must_== false
//      game9.situation.status must_== Some(Status.Draw)
//      game9.situation.winner must_== None
//    }
//
//    "keep incrementing halMovesSinceLastCapture after each player move" in {
//      game2.situation.board.history.halfMoveClock must_== 1
//      game3.situation.board.history.halfMoveClock must_== 2
//      game4.situation.board.history.halfMoveClock must_== 3
//      game5.situation.board.history.halfMoveClock must_== 4
//      game6.situation.board.history.halfMoveClock must_== 5
//      game8.situation.board.history.halfMoveClock must_== 7
//      game10.situation.board.history.halfMoveClock must_== 9
//    }
//
//    "keep being a draw even if in some weird case a neutral move could be played after" in {
//      game10.situation.end must_== true
//      game10.situation.playable(true) must_== false
//      game10.situation.status must_== Some(Status.Draw)
//      game10.situation.winner must_== None
//    }
//  }
//
//  /*
//   *      _ _ _ * *
//   *     _ _ _ 0 * *
//   *    _ _ _ _ 0 0 _
//   *   _ _ _ _ _ _ 0 _
//   *  _ _ _ _ 0 0 0 * *
//   *   _ _ _ _ _ _ _ _
//   *    _ _ _ _ _ _ _
//   *     _ _ _ _ _ _
//   *      _ _ _ _ _
//   */
//  "P2 and P1 moving left right left right then something else but then come back to the same position again" should {
//    val fenEndedGame = format.FEN("3ss/3Sss/4SS1/6S1/4SSSss/8/7/6/s4 0 0 b 0 0")
//    val board        = Board(fenEndedGame.pieces, History(score = Score(0, 0)), Abalone)
//    val situation    = Situation(board, P1)
//    val game         = Game.apply(board.variant)
//    val validMoves   = game.board.variant.validMoves(situation)
//    val game2        =
//      game.apply(validMoves(Pos.E5)(0)) // <- e5d5 0 is left in case there is a square on the left side
//    val validMoves2 = game2.board.variant.validMoves(game2.situation)
//    val game3       = game2.apply(
//      validMoves2(Pos.A1)(2)
//    ) // -> a1b1 moving right with index 2 because we can not move left (else, would be 3)
//    val validMoves3 = game3.board.variant.validMoves(game3.situation)
//    val game4       =
//      game3.apply(validMoves3(Pos.D5)(3)) // -> d5e5 3 is right in case we can move left, upLeft, upRight
//    val validMoves4  = game4.board.variant.validMoves(game4.situation)
//    val game5        = game4.apply(validMoves4(Pos.B1)(0)) // <- b1a1
//    val validMoves5  = game5.board.variant.validMoves(game5.situation)
//    val game6        = game5.apply(validMoves5(Pos.E5)(0)) // <- e5d5
//    val validMoves6  = game6.board.variant.validMoves(game6.situation)
//    val game7        = game6.apply(validMoves6(Pos.A1)(2)) // -> a1b1
//    val validMoves7  = game7.board.variant.validMoves(game7.situation)
//    val game8        = game7.apply(validMoves7(Pos.D5)(3)) // -> d5e5
//    val validMoves8  = game8.board.variant.validMoves(game8.situation)
//    val game9        = game8.apply(validMoves8(Pos.B1)(1)) // upLeft
//    val validMoves9  = game9.board.variant.validMoves(game9.situation)
//    val game10       = game9.apply(validMoves9(Pos.E5)(0)) // <- e5d5
//    val validMoves10 = game10.board.variant.validMoves(game10.situation)
//    val game11       = game10.apply(
//      validMoves10(Pos.B2)(5)
//    ) // downLeft to reach A1 -> // not a draw anymore, even though we saw the situation 2 times before
//    val validMoves11 = game11.board.variant.validMoves(game11.situation)
//    val game12       = game11.apply(validMoves11(Pos.D5)(3)) // -> d5e5
//
//    "not trigger a draw anymore" in {
//      game10.situation.end must_== false
//      game10.situation.playable(true) must_== true
//      game10.situation.status must_== None
//      game10.situation.winner must_== None
//
//      game11.situation.end must_== false
//      game11.situation.playable(true) must_== true
//      game11.situation.status must_== None
//      game11.situation.winner must_== None
//
//      game12.situation.end must_== false
//      game12.situation.playable(true) must_== true
//      game12.situation.status must_== None
//      game12.situation.winner must_== None
//    }
//  }
//
//  /*
//   *      _ _ _ * *
//   *     _ _ _ 0 * *
//   *    _ _ _ _ 0 0 _
//   *   _ _ _ _ _ _ 0 _
//   *  _ _ _ _ 0 0 0 * *
//   *   _ _ _ _ _ _ _ _
//   *    _ _ _ _ _ _ _
//   *     _ _ _ _ _ _
//   *     _ _ _ _ _
//   */
//  "P2 and P1 moving left right left right then something else but then come back to the same position again, and then do a 3fold repetition" should {
//    val fenEndedGame = format.FEN("3ss/3Sss/4SS1/6S1/4SSSss/8/7/6/s4 0 0 b 0 0")
//    val board        = Board(fenEndedGame.pieces, History(score = Score(0, 0)), Abalone)
//    val situation    = Situation(board, P1)
//    val game         = Game.apply(board.variant)
//    val validMoves   = game.board.variant.validMoves(situation)
//    val game2        =
//      game.apply(validMoves(Pos.E5)(0)) // <- e5d5 0 is left in case there is a square on the left side
//    val validMoves2 = game2.board.variant.validMoves(game2.situation)
//    val game3       = game2.apply(
//      validMoves2(Pos.A1)(2)
//    ) // -> a1b1 moving right with index 2 because we can not move left (else, would be 3)
//    val validMoves3 = game3.board.variant.validMoves(game3.situation)
//    val game4       =
//      game3.apply(validMoves3(Pos.D5)(3)) // -> d5e5 3 is right in case we can move left, upLeft, upRight
//    val validMoves4  = game4.board.variant.validMoves(game4.situation)
//    val game5        = game4.apply(validMoves4(Pos.B1)(0)) // <- b1a1
//    val validMoves5  = game5.board.variant.validMoves(game5.situation)
//    val game6        = game5.apply(validMoves5(Pos.E5)(0)) // <- e5d5
//    val validMoves6  = game6.board.variant.validMoves(game6.situation)
//    val game7        = game6.apply(validMoves6(Pos.A1)(2)) // -> a1b1
//    val validMoves7  = game7.board.variant.validMoves(game7.situation)
//    val game8        = game7.apply(validMoves7(Pos.D5)(3)) // -> d5e5
//    val validMoves8  = game8.board.variant.validMoves(game8.situation)
//    val game9        = game8.apply(validMoves8(Pos.B1)(1)) // upLeft
//    val validMoves9  = game9.board.variant.validMoves(game9.situation)
//    val game10       = game9.apply(validMoves9(Pos.E5)(0)) // <- e5d5
//    val validMoves10 = game10.board.variant.validMoves(game10.situation)
//    val game11       = game10.apply(
//      validMoves10(Pos.B2)(5)
//    ) // downLeft to reach A1 -> // not a draw anymore, even though we saw the situation 2 times before
//    val validMoves11 = game11.board.variant.validMoves(game11.situation)
//    val game12       = game11.apply(validMoves11(Pos.D5)(3)) // -> d5e5
//    val validMoves12 = game12.board.variant.validMoves(game12.situation)
//    val game13       = game12.apply(validMoves12(Pos.A1)(1)) // upRight to B2
//    val validMoves13 = game13.board.variant.validMoves(game13.situation)
//    val game14       = game13.apply(validMoves13(Pos.E5)(0)) // <- e5d5
//    val validMoves14 = game14.board.variant.validMoves(game14.situation)
//    val game15       = game14.apply(validMoves14(Pos.B2)(5)) // downLeft to reach A1
//    val validMoves15 = game15.board.variant.validMoves(game15.situation)
//    val game16       = game15.apply(validMoves15(Pos.D5)(3)) // -> d5e5
//    val validMoves16 = game16.board.variant.validMoves(game16.situation)
//    val game17       = game16.apply(validMoves16(Pos.A1)(1)) // upRight to B2 : draw
//
//    "trigger a draw only from AFTER the 3fold repetition sequence" in {
//      game16.situation.end must_== false
//      game16.situation.playable(true) must_== true
//      game16.situation.status must_== None
//      game16.situation.winner must_== None
//
//      game17.situation.end must_== true
//      game17.situation.playable(true) must_== false
//      game17.situation.status must_== Some(Status.Draw)
//      game17.situation.winner must_== None
//    }
//  }
//}
//
//class AbaloneVariantTestIsometry extends strategygames.chess.ChessTest {
//  val abaloneGameActionStrs = Vector(
//    Vector("a1d4"),
//    Vector("e9e6"),
//    Vector("b1d3"),
//    Vector("e1e4"),
//    Vector("c2e4"),
//    Vector("d8d6"),
//    Vector("b3e3"),
//    Vector("g3f4"),
//    Vector("i9f6"),
//    Vector("e8e5"),
//    Vector("a2c2"),
//    Vector("f2c2"),
//    Vector("h9f7"),
//    Vector("e2b2"),
//    Vector("a2b3"),
//    Vector("e7e4"),
//    Vector("i8g6"),
//    Vector("e6e3"),
//    Vector("b3e3"),
//    Vector("d7e6"),
//    Vector("h8h6"),
//    Vector("g4d4")
//  )
//
//  "Test Every move can be loaded from fen" in {
//    val gameFamily   = Abalone.gameFamily
//    val lib          = gameFamily.gameLogic
//    val stratVariant = StratVariant(lib, Abalone.key).get
//
//    _testEveryMoveLoadFenIsometry(lib, StratFen(lib, Abalone.initialFen.value), stratVariant)(
//      abaloneGameActionStrs.flatten.toList.map(uciStr => StratUci(lib, gameFamily, uciStr).get)
//    ) must beValid.like(gameData => {
//      val fen1 = StratForsyth.>>(lib, gameData.game)
//      val fen2 = StratForsyth.>>(lib, gameData.fenGame)
//      fen1 must_== fen2
//    })
//  }
}