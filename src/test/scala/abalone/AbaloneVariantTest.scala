package strategygames.abalone

import strategygames.abalone.variant.Abalone
import strategygames.format.{ FEN => StratFen, Forsyth => StratForsyth, Uci => StratUci }
import strategygames.variant.{ Variant => StratVariant }
import strategygames.{ Score, Status }

class AbaloneVariantTest extends AbaloneTest {
  /*
   *      · · · · ·
   *     · · · · · ·
   *    · · · · · · ·
   *   · · · · · · · ·
   *  · · · · ● ● ● o ·
   *   · · · · o · · ·
   *    · · · · · · ·
   *     · · · · · ·
   *      · · · · ·
   */
  "custom basic position" should {
    val fen         = format.FEN("5/6/7/8/4SSSs1/4s3/7/6/5 0 0 b 0 0")
    val board       = Board(fen.pieces(Abalone), History(), Abalone)
    val game        = new Game(Situation(board, P1))
    val validMoves  = valid(game)
    val moves       = validMoves.flatMap(_._2)
    val movesOf1    = moves.filter(of1(board))
    val movesOf23   = moves.filter(ofGt1(board))
    val game2       = next(game, validMoves, 6, 4, 4, 5)   // e7f5
    /*
     *      · · · · ·
     *     · · · · · ·
     *    · · · · · · ·
     *   · · · ● ● ● · ·
     *  · · · · - - - o ·
     *   · · · · o · · ·
     *    · · · · · · ·
     *     · · · · · ·
     *      · · · · ·
     */
    val validMoves2 = valid(game2)
    val game3       = next(game2, validMoves2, 7, 4, 6, 3) // e8d7
    /*
     *      · · · · ·
     *     · · · · · ·
     *    · · · · · · ·
     *   · · · ● ● ● · ·
     *  · · · · · · · - ·
     *   · · · · o · o ·
     *    · · · · · · ·
     *     · · · · · ·
     *      · · · · ·
     */
    val game4       = next(game3, 4, 5, 6, 4)              // f5e7
    /*
     *      · · · · ·
     *     · · · · · ·
     *    · · · · · · ·
     *   · · · - - - · ·
     *  · · · · ● ● ● · ·
     *   · · · · o · o ·
     *    · · · · · · ·
     *     · · · · · ·
     *      · · · · ·
     */
    val game5       = next(game4, 6, 3, 7, 4)              // d7e8
    /*
     *      · · · · ·
     *     · · · · · ·
     *    · · · · · · ·
     *   · · · · · · · ·
     *  · · · · ● ● ● o ·
     *   · · · · o · - ·
     *    · · · · · · ·
     *     · · · · · ·
     *      · · · · ·
     */
    val game6       = next(game5, 5, 4, 6, 3)              // e6d7
    /*
     *      · · · · ·
     *     · · · · · ·
     *    · · · · · · ·
     *   · · · · · · · ·
     *  · · · · ● - - o ·
     *   · · · · o ● ● ·
     *    · · · · · · ·
     *     · · · · · ·
     *      · · · · ·
     */
    val validMoves6 = valid(game6)

    "compute the correct number of moves" in {
      moves.size === movesOf1.size + movesOf23.size
      movesOf1.size === 11
      movesOf23.size === (1 + 1 + 5) + (1 + 1 + 2)
    }

    "compute the right set of moves using 2 or 3 marbles" in {
      ukeys(movesOf23) should contain("e5e9")
      ukeys(movesOf23) should contain("e5f8")
      ukeys(movesOf23) should contain("e5f7")
      ukeys(movesOf23) should contain("e7e4")
      ukeys(movesOf23) should contain("e7f5")
      ukeys(movesOf23) should contain("e7f6")
      ukeys(movesOf23) should contain("e6e4")
      ukeys(movesOf23) should contain("e6e9")
      ukeys(movesOf23) should contain("e6d7")
      ukeys(movesOf23) should contain("e6f8")
      ukeys(movesOf23) should contain("e6f5")
    }

    "side moves of 3 do move 3 marbles" in {
      val pieces2 = game2.situation.board.pieces.keys.map(_.key).toList
      pieces2.size === 5
      pieces2.contains("e5") === false
      pieces2.contains("e6") === false
      pieces2.contains("e7") === false
      pieces2.contains("f5") === true
      pieces2.contains("f6") === true
      pieces2.contains("f7") === true
      pieces2.contains("d5") === true
      pieces2.contains("e8") === true

      ukeys(validMoves2, 4, 3) should contain("d5e5")
      ukeys(validMoves2, 4, 3) should contain("d5e6")
      ukeys(validMoves2, 7, 4) should contain("e8e7")

      board.pieces === game5.situation.board.pieces
      game5.situation.board.pieces.size === 5
    }

    "side moves of 2 do move 2 marbles" in {
      val pieces6 = game6.situation.board.pieces.keys.map(_.key).toList
      pieces6.size === 5
      pieces6.contains("d5") === true
      pieces6.contains("d6") === true
      pieces6.contains("d7") === true
      pieces6.contains("e5") === true
      pieces6.contains("e6") === false
      pieces6.contains("e7") === false
      pieces6.contains("e8") === true

      ukeys(validMoves6, 4, 3) should not contain "d5d8"
      ukeys(validMoves6, 4, 3) should not contain "d5e5"
      ukeys(validMoves6, 4, 3) should contain("d5e6")
      ukeys(validMoves6, 7, 4) should contain("e8e7")
    }
  }

  /*
   *     o o · ● ●
   *    o o o ● ● ●
   *   · o o · ● ● ·
   *  · · · · · · · ·
   * · · · · · · · · ·
   *  · · · · · · · ·
   *   · ● ● · o o ·
   *    ● ● ● o o o
   *     ● ● · o o
   */
  "\"Belgian daisy\" start position" should {
    val fen        = Abalone.initialFen
    val board      = Board(fen.pieces(Abalone), History(), Abalone)
    val situation  = Situation(board, P1)
    val validMoves = valid(situation)
    val moves      = validMoves.flatMap(_._2)
    val lineMoves  = valid_line(situation).flatMap(_._2)
    val jumpMoves  = valid_jump(situation).flatMap(_._2)
    val movesOf1   = moves.filter(of1(board))
    val movesOf23  = moves.filter(ofGt1(board))

    "compute the correct number of moves" in {
      moves.size === 52

      lineMoves.size === 44
      jumpMoves.size === 8

      movesOf1.size === 20
      movesOf23.size === 32

      validMoves.get(new Pos(0, 0)).get.filter(ofGt1(board)).size === 3
      validMoves.get(new Pos(1, 0)).get.filter(ofGt1(board)).size === 2
      validMoves.get(new Pos(0, 1)).get.filter(ofGt1(board)).size === 2
      validMoves.get(new Pos(1, 1)).get.filter(ofGt1(board)).size === 2
      validMoves.get(new Pos(1, 2)).get.filter(ofGt1(board)).size === 2
      validMoves.get(new Pos(2, 1)).get.filter(ofGt1(board)).size === 2
      validMoves.get(new Pos(2, 2)).get.filter(ofGt1(board)).size === 3
    }
  }

  /*
   *     ● ● ● ● ●
   *    ● · · · · ·
   *   ● · · · · · ·
   *  ● · o o o o o ·
   * · ● · · · · · o ·
   *  · ● ● ● ● ● · o
   *   · · · · · · o
   *    · · · · · o
   *     o o o o o
   */
  "\"Snakes variant\" start position" should {
    val fen       = format.FEN("SSSSS/S5/S6/S1sssss1/1S5s1/1SSSSS1s/6s/5s/sssss 0 0 b 0 0")
    val board     = Board(fen.pieces(Abalone), History(), Abalone)
    val situation = Situation(board, P1)
    val moves     = valid(situation).flatMap(_._2)
    val lineMoves = valid_line(situation).flatMap(_._2)
    val jumpMoves = valid_jump(situation).flatMap(_._2)
    val movesOf1  = moves.filter(of1(board))
    val movesOf23 = moves.filter(ofGt1(board))

    "compute the correct number of moves" in {
      moves.size === 98

      lineMoves.size === 48
      jumpMoves.size === 50

      movesOf1.size === 40
      movesOf23.size === 58
    }
  }

  /*
   *     ● · ● · ●
   *    · ● o o ● ·
   *   · ● o ● o ● ·
   *  · · · ● ● · · ·
   * · · · · · · · · ·
   *  · · · o o · · ·
   *   · o ● o ● o ·
   *    · o ● ● o ·
   *     o · o · o
   */
  "\"Alien\" start position" should {
    val fen       = format.FEN("S1S1S/1SssS1/1SsSsS1/3SS3/9/3ss3/1sSsSs1/1sSSs1/s1s1s 0 0 b 0 0")
    val board     = Board(fen.pieces(Abalone), History(), Abalone)
    val situation = Situation(board, P1)
    val moves     = valid(situation).flatMap(_._2)
    val lineMoves = valid_line(situation).flatMap(_._2)
    val jumpMoves = valid_jump(situation).flatMap(_._2)
    val movesOf1  = moves.filter(of1(board))
    val movesOf23 = moves.filter(ofGt1(board))

    "compute the correct number of moves" in {
      moves.size === 48

      lineMoves.size === 44
      jumpMoves.size === 4

      movesOf1.size === 28
      movesOf23.size === 20
    }
  }

  /*
   *     · · · · ·
   *    o · · · · ●
   *   o o · · · ● ●
   *  o o o o · ● ● ●
   * · · · ● · ● · · ·
   *  ● ● ● · o o o o
   *   ● ● · · · o օ
   *    ● · · · · օ
   *     · · · · ·
   */
  "\"Domination\" start position" should {
    val fen        = format.FEN("5/s4S/ss3SS/ssss1SSS/3S1S3/SSS1ssss/SS3ss/S4s/5 0 0 b 0 0")
    val board      = Board(fen.pieces(Abalone), History(), Abalone)
    val situation  = Situation(board, P1)
    val validMoves = valid(situation)
    val moves      = validMoves.flatMap(_._2)
    val lineMoves  = valid_line(situation).flatMap(_._2)
    val jumpMoves  = valid_jump(situation).flatMap(_._2)
    val movesOf1   = moves.filter(of1(board))
    val movesOf23  = moves.filter(ofGt1(board))

    "compute the correct number of moves" in {
      moves.size === 80

      lineMoves.size === 52
      jumpMoves.size === 28

      movesOf1.size === 28
      movesOf23.size === 52
    }

    "find the moves pushing marbles" in {
      val of23: Move => Boolean = m => board.variant.boardType.norm(m.dest - m.orig) > 1

      ukeys(validMoves.get(new Pos(1, 2)).get.filter(of23)) should contain("c2g6")
      ukeys(validMoves.get(new Pos(2, 3)).get.filter(of23)) should contain("d3g6")
      ukeys(validMoves.get(new Pos(7, 6)).get.filter(of23)) should contain("g8c4")
      ukeys(validMoves.get(new Pos(6, 5)).get.filter(of23)) should contain("f7c4")
    }
  }

  /*
   *     · · o · o
   *    · · · ● o ·
   *   · · · · · · ·
   *  · · · · · · · ·
   * · · · · ● ● ● · ·
   *  o o ● ● · · ● ·
   *   · · · o · · ●
   *    · · o ● o o
   *     · o · · ·
   */
  "custom start position with a total of 29 marbles" should {
    val fen        = format.FEN("2s1s/3Ss1/7/8/4SSS2/ssSS2S1/3s2S/2sSss/1s3 5 5 b 0 0")
    val board      = Board(fen.pieces(Abalone), History(score = Score(5, 5)), Abalone)
    val situation  = Situation(board, P1)
    val validMoves = valid(situation)
    val moves      = validMoves.flatMap(_._2)
    val lineMoves  = valid_line(situation).flatMap(_._2)
    val jumpMoves  = valid_jump(situation).flatMap(_._2)
    val movesOf1   = moves.filter(of1(board))
    val movesOf23  = moves.filter(ofGt1(board))

    "compute the correct number of moves" in {
      moves.size === 58

      lineMoves.size === 41
      jumpMoves.size === 17

      movesOf1.size === 32
      movesOf23.size === 26
    }

    val situation_p2  = Situation(board, P2)
    val validMoves_p2 = board.variant.validMoves(situation_p2)
    val moves_p2      = validMoves_p2.flatMap(_._2)
    val lineMoves_p2  = board.variant.validMoves_line(situation_p2).flatMap(_._2)
    val jumpMoves_p2  = board.variant.validMoves_jump(situation_p2).flatMap(_._2)
    val movesOf1_p2   = moves_p2.filter(of1(board))
    val movesOf23_p2  = moves_p2.filter(ofGt1(board))

    "compute the correct number of moves for P2" in {
      moves_p2.size === 44

      lineMoves_p2.size === 34
      jumpMoves_p2.size === 10

      movesOf1_p2.size === 31
      movesOf23_p2.size === 13
    }
  }

  /*
   *     ● ● ● · ·
   *    · · o ● ● ●
   *   · ● ● o o o o
   *  · · · ● · · · ·
   * · · · ● · · · · ·
   *  · · ● · · · · ·
   *   · o · · · · ·
   *    o o o o · ·
   *     ● ● ● ● o
   */
  "custom start position" should {
    val fen        = format.FEN("SSS2/2sSSS/1SSssss/3S4/3S5/2S5/1s5/ssss2/SSSSs 5 5 b 0 0")
    val board      = Board(fen.pieces(Abalone), History(), Abalone)
    val situation  = Situation(board, P1)
    val validMoves = valid(situation)
    val lineMoves  = valid_line(situation).flatMap(_._2)
    val jumpMoves  = valid_jump(situation).flatMap(_._2)
    val moves      = validMoves.flatMap(_._2)
    val movesOf1   = moves.filter(of1(board))
    val movesOf23  = moves.filter(ofGt1(board))

    "compute the correct number of moves" in {
      moves.size === 53

      lineMoves.size === 38
      jumpMoves.size === 15

      movesOf1.size === 27
      movesOf23.size === 26
    }

    "not generate a move for a marble that is blocked" in {
      moves.filter(m => m.orig.equals(0, 0)) should beEmpty
      movesOf1.filter(m => m.orig.equals(0, 0)) should beEmpty
      movesOf23.filter(m => m.orig.equals(0, 0)) should beEmpty
    }

    "find and compute all moves that push off (3 vs 2, 3 vs 1, 2 vs 1)" in {
      ukeys(validMoves, 1, 0) should contain("a2a5")
      ukeys(validMoves, 2, 0) should contain("a3a5")
      ukeys(validMoves, 4, 5) should contain("f5b1")
    }
  }

  /*
   *     o · · · ·
   *    ● o · · · ·
   *   ● · o · · · ·
   *  ● · · ● · · · ·
   * o · · · ● · · · ·
   *  o · · · ● · · ·
   *   ● · · · ● · ·
   *    ● o o o o o
   *     ● o ● ● ●
   */
  "special position to test some edge cases" should {
    val fen        = format.FEN("s4/Ss4/S1s4/S2S4/s3S4/s3S3/S3S2/Ssssss/SsSSS 5 5 b 0 0")
    val board      = Board(fen.pieces(Abalone), History(), Abalone)
    val validMoves = valid(Situation(board, P1))

    "not generate a push when a marble of the same colour blocks it (oooxo)" in {
      validMoves.get(new Pos(4, 4)).get.map(_.toUci.keys) should not contain "e5a5"
      validMoves.get(new Pos(4, 0)).get.map(_.toUci.keys) should not contain "a5a1"
    }

    "produce no move from a marble or a group or marbles that are stuck" in {
      ukeys(validMoves, 2, 0) should beEmpty
      ukeys(validMoves, 3, 0) should beEmpty
      ukeys(validMoves, 4, 0) should beEmpty // (oooxo)
    }

    "not move more than 3 marbles" in {
      ukeys(validMoves, 4, 2).size === 7
      ukeys(validMoves, 4, 2) should not contain "c5i5"
      ukeys(validMoves, 4, 2) should not contain "c5g6"
      ukeys(validMoves, 4, 2) should not contain "c5f4"
    }

    "find and compute all moves that push off (3 vs 2, 3 vs 1, 2 vs 1)" in {
      ukeys(validMoves, 3, 7) should contain("h4e1")
      ukeys(validMoves, 2, 6) should contain("g3e1")
      ukeys(validMoves, 2, 6) should contain("g3i5")
      ukeys(validMoves, 1, 5) should contain("f2i5")

      ukeys(validMoves, 0, 0) should contain("a1e1")
      ukeys(validMoves, 0, 0) should not contain "a1a5"
    }

    "not create a move in case of n vs n push" in {
      ukeys(validMoves, 0, 1) should not contain "b1e1"
      ukeys(validMoves, 4, 3) should not contain "d5i5"
    }
  }

  /*
   *     · · · · ·
   *    · · · · · ·
   *   · · · · · · ·
   *  · · · · · · · ·
   * · · · · · ● ● ● o
   *  · · · · · · · ·
   *   · · · · · · ·
   *    · · · · · ·
   *     · · · · ·
   */
  "pushing out the only marble of P2 with a score of 0" should {
    val fenEndedGame = format.FEN("5/6/7/8/5SSSs/8/7/6/5 0 0 b 0 0")
    val board        = Board(fenEndedGame.pieces(Abalone), History(), Abalone)
    val game2        = next(new Game(Situation(board, P1)), 5, 4, 8, 4) // e6e9

    "increment the score of P1" in {
      board.history.score === Score()
      game2.situation.board.history.score === Score(p1 = 1)
    }

    "trigger a loss for P2 (no marbles left to move)" in {
      game2.situation.end === true
      game2.situation.staleMate === false
      game2.situation.playable(true) === false
      game2.situation.status === Some(Status.VariantEnd)
      game2.situation.winner === Some(P1)
    }
  }

  /*
   *     · · · · ·
   *    · · · · · ·
   *   · · · · · · ·
   *  · · · · · · · ·
   * · · · · · ● ● ● o
   *  · · · · · · · ·
   *   · · · · · · ·
   *    · · · · · ·
   *     · · · · ·
   */
  "pushing out the only marble of P2 with a score of 5" should {
    val fenEndedGame = format.FEN("5/6/7/8/5SSSs/8/7/6/5 5 0 b 0 0")
    val board        = Board(fenEndedGame.pieces(Abalone), History(score = Score(p1 = 5)), Abalone)
    val game         = new Game(Situation(board, P1))
    val game2        = next(game, 5, 4, 8, 4) // e6e9

    "increment the score of P1" in {
      board.history.score === Score(p1 = 5) // game.situation.board.history.score is Score(0,0)
      game2.situation.board.history.score === Score(p1 = 6)
    }

    "trigger a win condition defined by the variant (score 6, P2 has no marbles)" in {
      game2.situation.end === true
      game2.situation.staleMate === false
      game2.situation.playable(true) === false
      game2.situation.status === Some(Status.VariantEnd)
      game2.situation.winner === Some(P1)
    }
  }

  /*
   *      · · · o o
   *     · · · ● o o
   *    · · · · ● ● ·
   *   · · · · · · ● ·
   *  · · · · ● ● ● o o
   *   · · · · · · · ·
   *    · · · · · · ·
   *     · · · · · ·
   *      o · · ·
   */
  "P1 pushing out 6 marbles consecutively" should {
    val fenEndedGame = format.FEN("3ss/3Sss/4SS1/6S1/4SSSss/8/7/6/s4 0 0 b 0 0")
    val board        = Board(fenEndedGame.pieces(Abalone), History(), Abalone)
    val game         = new Game(Situation(board, P1))
    val game2        = next(game, 4, 4, 8, 4)   // e5e9
    /*
     *      · · · o o
     *     · · · ● o o
     *    · · · · ● ● ·
     *   · · · · · · ● ·
     *  · · · · - ● ● ● o
     *   · · · · · · · ·
     *    · · · · · · ·
     *     · · · · · ·
     *      o · · ·
     */
    val game3        = next(game2, 0, 0, 0, 1)  // a1b1
    /*
     *      · · · o o
     *     · · · ● o o
     *    · · · · ● ● ·
     *   · · · · · · ● ·
     *  · · · · · ● ● ● o
     *   · · · · · · · ·
     *    · · · · · · ·
     *     o · · · · ·
     *      - · · ·
     */
    val game4        = next(game3, 5, 4, 8, 4)  // e6e9
    /*
     *      · · · o o
     *     · · · ● o o
     *    · · · · ● ● ·
     *   · · · · · · ● ·
     *  · · · · · - ● ● ●
     *   · · · · · · · ·
     *    · · · · · · ·
     *     o · · · · ·
     *      · · · ·
     */
    val game5        = next(game4, 0, 1, 0, 0)  // b1a1
    /*
     *      · · · o o
     *     · · · ● o o
     *    · · · · ● ● ·
     *   · · · · · · ● ·
     *  · · · · · · ● ● ●
     *   · · · · · · · ·
     *    · · · · · · ·
     *     - · · · · ·
     *      o · · ·
     */
    val game6        = next(game5, 7, 4, 7, 8)  // e8i8
    /*
     *      · · · o o
     *     · · · ● ● o
     *    · · · · ● ● ·
     *   · · · · · · ● ·
     *  · · · · · · ● - ●
     *   · · · · · · · ·
     *    · · · · · · ·
     *     · · · · · ·
     *      o · · ·
     */
    val game7        = next(game6, 0, 0, 0, 1)  // a1b1
    /*
     *      · · · o o
     *     · · · ● ● o
     *    · · · · ● ● ·
     *   · · · · · · ● ·
     *  · · · · · · ● · ●
     *   · · · · · · · ·
     *    · · · · · · ·
     *     o · · · · ·
     *      - · · ·
     */
    val game8        = next(game7, 7, 5, 7, 8)  // f8i8
    /*
     *      · · · ● o
     *     · · · ● ● o
     *    · · · · ● ● ·
     *   · · · · · · - ·
     *  · · · · · · ● · ●
     *   · · · · · · · ·
     *    · · · · · · ·
     *     o · · · · ·
     *      · · · ·
     */
    val game9        = next(game8, 0, 1, 0, 0)  // b1a1
    /*
     *      · · · ● o
     *     · · · ● ● o
     *    · · · · ● ● ·
     *   · · · · · · · ·
     *  · · · · · · ● · ●
     *   · · · · · · · ·
     *    · · · · · · ·
     *     - · · · · ·
     *      o · · ·
     */
    val game10       = next(game9, 6, 7, 8, 7)  // h7h9
    /*
     *      · · · ● o
     *     · · · - ● ●
     *    · · · · ● ● ·
     *   · · · · · · · ·
     *  · · · · · · ● · ●
     *   · · · · · · · ·
     *    · · · · · · ·
     *     · · · · · ·
     *      o · · ·
     */
    val game11       = next(game10, 0, 0, 0, 1) // a1b1
    /*
     *      · · · ● o
     *     · · · · ● ●
     *    · · · · ● ● ·
     *   · · · · · · · ·
     *  · · · · · · ● · ●
     *   · · · · · · · ·
     *    · · · · · · ·
     *     o · · · · ·
     *      - · · ·
     */
    val game12       = next(game11, 6, 6, 8, 8) // g7i9
    /*
     *      · · · ● ●
     *     · · · · ● ●
     *    · · · · - ● ·
     *   · · · · · · · ·
     *  · · · · · · ● · ●
     *   · · · · · · · ·
     *    · · · · · · ·
     *     o · · · · ·
     *      · · · ·
     */

    "should be a valid board at beginning and end" in {
      game.board.valid(true) === true
      game2.board.valid(true) === true
      game11.board.valid(true) === true
      game12.board.valid(true) === true
    }

    "should enter a non reversible state only when pushing out" in {
      game8.board.variant
        .isIrreversible(game8.situation, game9.situation.board.history.lastAction.get) === false
      game9.board.variant
        .isIrreversible(game9.situation, game10.situation.board.history.lastAction.get) === true
      game10.board.variant
        .isIrreversible(game10.situation, game11.situation.board.history.lastAction.get) === false
    }

    "increment the score of P1 each time he plays a move" in {
      board.history.score === Score()
      game2.situation.board.history.score === Score(p1 = 1)
      game4.situation.board.history.score === Score(p1 = 2)
      game6.situation.board.history.score === Score(p1 = 3)
      game8.situation.board.history.score === Score(p1 = 4)
      game10.situation.board.history.score === Score(p1 = 5)
      game12.situation.board.history.score === Score(p1 = 6)
    }

    "reset the halfMovesSinceLastCapture after P1 push out" in {
      game2.situation.board.history.halfMoveClock === 0
      game3.situation.board.history.halfMoveClock === 1
      game4.situation.board.history.halfMoveClock === 0
      game5.situation.board.history.halfMoveClock === 1
      game6.situation.board.history.halfMoveClock === 0
      game8.situation.board.history.halfMoveClock === 0
      game10.situation.board.history.halfMoveClock === 0
      game12.situation.board.history.halfMoveClock === 0
    }

    "reduce the number of marbles on the board after P1 push out" in {
      board.pieces.size === 7 + 7
      game2.situation.board.pieces.size === 7 + 6
      game4.situation.board.pieces.size === 7 + 5
      game6.situation.board.pieces.size === 7 + 4
      game8.situation.board.pieces.size === 7 + 3
      game10.situation.board.pieces.size === 7 + 2
      game12.situation.board.pieces.size === 7 + 1
    }

    "trigger a win condition defined by the variant, and no stalemate is detected" in {
      game12.situation.end === true
      game12.situation.staleMate === false
      game12.situation.playable(true) === false
      game12.situation.status === Some(Status.VariantEnd)
      game12.situation.winner === Some(P1)
    }
  }

  /*
   *      · · · o o
   *     · · · ● o o
   *    · · · · ● ● ·
   *   · · · · · · ● ·
   *  · · · · ● ● ● o o
   *   · · · · · · · ·
   *    · · · · · · ·
   *     · · · · · ·
   *      o · · · ·
   */
  "P2 and P1 moving left right right left" should {
    val fenEndedGame = format.FEN("3ss/3Sss/4SS1/6S1/4SSSss/8/7/6/s4 0 0 b 0 0")
    val board        = Board(fenEndedGame.pieces(Abalone), History(), Abalone)

    val game  = new Game(Situation(board, P1))
    val game2 = next(game, 4, 4, 3, 4)  // e5e4 left
    val game3 = next(game2, 0, 0, 1, 0) // a1a2 right
    val game4 = next(game3, 3, 4, 4, 4) // e4e5 right
    val game5 = next(game4, 1, 0, 0, 0) // a2a1 left <- game
    val game6 = next(game5, 4, 4, 3, 4) // e5e4 left <- game2
    val game7 = next(game6, 0, 0, 1, 0) // a1a2 right <- game3
    val game8 = next(game7, 3, 4, 4, 4) // e4e5 right <- game4

    val game9   = next(game8, 1, 0, 0, 0) // a2a1 left <- game5, game: DRAW !
    val game10  = next(game9, 4, 4, 3, 4) // e5e4 left <- game6, game2: DRAW !
    val game10a = next(game9, 4, 4, 5, 5) // e5f6 right

    val game9b  = next(game8, 1, 0, 1, 1)   // a2b2 up left
    val game10b = next(game9b, 4, 4, 3, 4)  // e5e4 left
    val game11b = next(game10b, 1, 1, 0, 0) // b2a1 down left <- game6, game2, != player to move: NOT A DRAW
    val game12b = next(game11b, 3, 4, 4, 4) // e4e5 right <- game5, game, != player to move: NOT A DRAW

    "if more than once, trigger a draw by repetition after seeing the same situation for the 3rd time, and no stalemate is detected" in {
      game8.situation.end === false
      game8.situation.playable(true) === true

      game9.situation.end === true
      game9.situation.staleMate === false
      game9.situation.playable(true) === false
      game9.situation.status === Some(Status.Draw)
      game9.situation.winner === None
    }

    "keep incrementing halfMovesSinceLastCapture after each player move" in {
      game2.situation.board.history.halfMoveClock === 1
      game3.situation.board.history.halfMoveClock === 2
      game4.situation.board.history.halfMoveClock === 3
      game5.situation.board.history.halfMoveClock === 4
      game6.situation.board.history.halfMoveClock === 5
      game8.situation.board.history.halfMoveClock === 7
      game10.situation.board.history.halfMoveClock === 9
    }

    "if more than once, correctly updates the status even if in some weird case a move could be played after the first draw" in {
      game10.situation.end === true
      game10.situation.playable(true) === false
      game10.situation.status === Some(Status.Draw)
      game10.situation.winner === None

      game10a.situation.end === false
      game10a.situation.playable(true) === true
      game10a.situation.status === None
      game10a.situation.winner === None
    }

    "if at most once, not trigger a draw" in {
      game10b.situation.end === false
      game10b.situation.playable(true) === true
      game10b.situation.status === None
      game10b.situation.winner === None

      game11b.situation.end === false
      game11b.situation.playable(true) === true
      game11b.situation.status === None
      game11b.situation.winner === None

      game12b.situation.end === false
      game12b.situation.playable(true) === true
      game12b.situation.status === None
      game12b.situation.winner === None
    }
  }

  class AbaloneVariantTestIsometry extends strategygames.chess.ChessTest {
    val abaloneGameActionStrs = Vector(
      Vector("a1d4"),
      Vector("e9e6"), // FIXME Wtf?
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

    "Test every move can be loaded from fen" in {
      val gameFamily   = Abalone.gameFamily
      val lib          = gameFamily.gameLogic
      val stratVariant = StratVariant(lib, Abalone.key).get

      _testEveryMoveLoadFenIsometry(lib, StratFen(lib, Abalone.initialFen.value), stratVariant)(
        abaloneGameActionStrs.flatten.toList.map(uciStr => StratUci(lib, gameFamily, uciStr).get)
      ) must beValid.like(gameData => {
        val fen1 = StratForsyth.>>(lib, gameData.game)
        val fen2 = StratForsyth.>>(lib, gameData.fenGame)
        fen1 === fen2
      })
    }
  }
}
