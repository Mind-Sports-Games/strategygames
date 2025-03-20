package strategygames.abalone

import org.specs2.matcher.ValidatedMatchers
import strategygames.{Score, Status}
//import strategygames.{ Score, Status }
//import strategygames.format.{ FEN => StratFen, Forsyth => StratForsyth, Uci => StratUci }
//import strategygames.variant.{ Variant => StratVariant }
import strategygames.abalone.variant.Abalone

class AbaloneVariantTest extends AbaloneTest with ValidatedMatchers {
  def of1(board: Board): Move => Boolean = m => board.variant.boardType.norm(m.dest - m.orig) == 1

  def of23(board: Board): Move => Boolean = m => board.variant.boardType.norm(m.dest - m.orig) > 1

  def valid(game: Game): Map[Pos, List[Move]] = valid(game.situation)

  def valid(sit: Situation): Map[Pos, List[Move]] = sit.board.variant.validMoves(sit)

  def valid_line(sit: Situation): Map[Pos, List[Move]] = sit.board.variant.validMoves_line(sit)

  def valid_jump(sit: Situation): Map[Pos, List[Move]] = sit.board.variant.validMoves_jump(sit)

  def next(game: Game, fx: Int, fy: Int, tx: Int, ty: Int): Game =
    next(game, valid(game), fx, fy, tx, ty)

  def next(game: Game, validMoves: Map[Pos, List[Move]], fx: Int, fy: Int, tx: Int, ty: Int): Game =
    next(game, validMoves, new Pos(fx, fy), new Pos(tx, ty))

  def next(game: Game, validMoves: Map[Pos, List[Move]], orig: Pos, dest: Pos): Game =
    game(validMoves(orig).find(m => m.dest == dest).get)

  def ukeys(validMoves: Map[Pos, List[Move]], fx: Int, fy: Int): Iterable[String] =
    ukeys(validMoves, new Pos(fx, fy))

  def ukeys(validMoves: Map[Pos, List[Move]], orig: Pos): Iterable[String] = ukeys(validMoves.get(orig).get)

  def ukeys(moves: Iterable[Move]): Iterable[String] = moves.map(_.toUci.keys)

  /*
   *      · · · · ·
   *     · · · · · ·
   *    · · · · · · ·
   *   · · · · · · · ·
   *  · · · · 1 1 1 2 ·
   *   · · · · 2 · · ·
   *    · · · · · · ·
   *     · · · · · ·
   *      · · · · ·
   */
  "custom basic position" should {
    val fen         = format.FEN("5/6/7/4s3/4SSSs1/8/7/6/5 0 0 b 0 0")
    val board       = Board(fen.pieces(Abalone.boardType), History(), Abalone)
    val game        = new Game(Situation(board, P1))
    val validMoves  = valid(game)
    val moves       = validMoves.flatMap(_._2)
    val movesOf1    = moves.filter(of1(board))
    val movesOf23   = moves.filter(of23(board))
    val game2       = next(game, validMoves, 6, 4, 4, 5)   // e7f5
    /*
     *      · · · · ·
     *     · · · · · ·
     *    · · · · · · ·
     *   · · · 1 1 1 · ·
     *  · · · · - - - 2 ·
     *   · · · · 2 · · ·
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
     *   · · · 1 1 1 · ·
     *  · · · · · · · - ·
     *   · · · · 2 · 2 ·
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
     *  · · · · 1 1 1 · ·
     *   · · · · 2 · 2 ·
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
     *  · · · · 1 1 1 2 ·
     *   · · · · 2 · - ·
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
     *  · · · · 1 - - 2 ·
     *   · · · · 2 1 1 ·
     *    · · · · · · ·
     *     · · · · · ·
     *      · · · · ·
     */
    val validMoves6 = valid(game6)

    "compute the correct number of moves" in {
      moves.size must_== movesOf1.size + movesOf23.size
      movesOf1.size must_== 11
      movesOf23.size must_== (1 + 1 + 5) + (1 + 1 + 2)
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
      pieces2.size must_== 5
      pieces2.contains("e5") must_== false
      pieces2.contains("e6") must_== false
      pieces2.contains("e7") must_== false
      pieces2.contains("f5") must_== true
      pieces2.contains("f6") must_== true
      pieces2.contains("f7") must_== true
      pieces2.contains("d5") must_== true
      pieces2.contains("e8") must_== true

      ukeys(validMoves2, 4, 3) should contain("d5e5")
      ukeys(validMoves2, 4, 3) should contain("d5e6")
      ukeys(validMoves2, 7, 4) should contain("e8e7")

      board.pieces must_== game5.situation.board.pieces
      game5.situation.board.pieces.size must_== 5
    }

    "side moves of 2 do move 2 marbles" in {
      val pieces6 = game6.situation.board.pieces.keys.map(_.key).toList
      pieces6.size must_== 5
      pieces6.contains("d5") must_== true
      pieces6.contains("d6") must_== true
      pieces6.contains("d7") must_== true
      pieces6.contains("e5") must_== true
      pieces6.contains("e6") must_== false
      pieces6.contains("e7") must_== false
      pieces6.contains("e8") must_== true

      ukeys(validMoves6, 4, 3) should not contain "d5d8"
      ukeys(validMoves6, 4, 3) should not contain "d5e5"
      ukeys(validMoves6, 4, 3) should contain("d5e6")
      ukeys(validMoves6, 7, 4) should contain("e8e7")
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
    val validMoves = valid(situation)
    val moves      = validMoves.flatMap(_._2)
    val lineMoves  = valid_line(situation).flatMap(_._2)
    val jumpMoves  = valid_jump(situation).flatMap(_._2)
    val movesOf1   = moves.filter(of1(board))
    val movesOf23  = moves.filter(of23(board))

    "compute the correct number of moves" in {
      moves.size must_== 52

      lineMoves.size must_== 44
      jumpMoves.size must_== 8

      movesOf1.size must_== 20
      movesOf23.size must_== 32

      validMoves.get(new Pos(0, 0)).get.filter(of23(board)).size must_== 3
      validMoves.get(new Pos(1, 0)).get.filter(of23(board)).size must_== 2
      validMoves.get(new Pos(0, 1)).get.filter(of23(board)).size must_== 2
      validMoves.get(new Pos(1, 1)).get.filter(of23(board)).size must_== 2
      validMoves.get(new Pos(1, 2)).get.filter(of23(board)).size must_== 2
      validMoves.get(new Pos(2, 1)).get.filter(of23(board)).size must_== 2
      validMoves.get(new Pos(2, 2)).get.filter(of23(board)).size must_== 3
    }
  }

  "\"Snakes variant\" start position" should {
    val fen       = format.FEN("sssss/5s/6s/1SSSSS1s/1S5s1/S1sssss1/S6/S5/SSSSS 0 0 b 0 0")
    val board     = Board(fen.pieces(Abalone.boardType), History(), Abalone)
    val situation = Situation(board, P1)
    val moves     = valid(situation).flatMap(_._2)
    val lineMoves = valid_line(situation).flatMap(_._2)
    val jumpMoves = valid_jump(situation).flatMap(_._2)
    val movesOf1  = moves.filter(of1(board))
    val movesOf23 = moves.filter(of23(board))

    "compute the correct number of moves" in {
      moves.size must_== 98

      lineMoves.size must_== 48
      jumpMoves.size must_== 50

      movesOf1.size must_== 40
      movesOf23.size must_== 58
    }
  }

  "\"Alien\" start position" should {
    val fen       = format.FEN("s1s1s/1sSSs1/1sSsSs1/3ss3/9/3SS3/1SsSsS1/1SssS1/S1S1S 0 0 b 0 0")
    val board     = Board(fen.pieces(Abalone.boardType), History(), Abalone)
    val situation = Situation(board, P1)
    val moves     = valid(situation).flatMap(_._2)
    val lineMoves = valid_line(situation).flatMap(_._2)
    val jumpMoves = valid_jump(situation).flatMap(_._2)
    val movesOf1  = moves.filter(of1(board))
    val movesOf23 = moves.filter(of23(board))

    "compute the correct number of moves" in {
      moves.size must_== 48

      lineMoves.size must_== 44
      jumpMoves.size must_== 4

      movesOf1.size must_== 28
      movesOf23.size must_== 20
    }
  }

  "\"Domination\" start position" should {
    val fen        = format.FEN("5/S4s/SS3ss/SSS1ssss/3S1S3/ssss1SSS/ss3SS/s4S/5 0 0 b 0 0")
    val board      = Board(fen.pieces(Abalone.boardType), History(), Abalone)
    val situation  = Situation(board, P1)
    val validMoves = valid(situation)
    val moves      = validMoves.flatMap(_._2)
    val lineMoves  = valid_line(situation).flatMap(_._2)
    val jumpMoves  = valid_jump(situation).flatMap(_._2)
    val movesOf1   = moves.filter(of1(board))
    val movesOf23  = moves.filter(of23(board))

    "compute the correct number of moves" in {
      moves.size must_== 80

      lineMoves.size must_== 52
      jumpMoves.size must_== 28

      movesOf1.size must_== 28
      movesOf23.size must_== 52
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
   *     · · 2 · 2
   *    · · · 1 2 ·
   *   · · · · · · ·
   *  · · · · · · · ·
   * · · · · 1 1 1 · ·
   *  2 2 1 1 · · 1 ·
   *   · · · 2 · · 1
   *    · · 2 1 2 2
   *     · 2 · · ·
   */
  "custom start position with a total of 29 marbles" should {
    val fen        = format.FEN("1s3/2sSss/3s2S/ssSS2S1/4SSS2/8/7/3Ss1/2s1s 5 5 b 0 0")
    val board      = Board(fen.pieces(Abalone.boardType), History(score = Score(5, 5)), Abalone)
    val situation  = Situation(board, P1)
    val validMoves = valid(situation)
    val moves      = validMoves.flatMap(_._2)
    val lineMoves  = valid_line(situation).flatMap(_._2)
    val jumpMoves  = valid_jump(situation).flatMap(_._2)
    val movesOf1   = moves.filter(of1(board))
    val movesOf23  = moves.filter(of23(board))

    "compute the correct number of moves" in {
      moves.size must_== 58

      lineMoves.size must_== 41
      jumpMoves.size must_== 17

      movesOf1.size must_== 32
      movesOf23.size must_== 26
    }

    val situation_p2  = Situation(board, P2)
    val validMoves_p2 = board.variant.validMoves(situation_p2)
    val moves_p2      = validMoves_p2.flatMap(_._2)
    val lineMoves_p2  = board.variant.validMoves_line(situation_p2).flatMap(_._2)
    val jumpMoves_p2  = board.variant.validMoves_jump(situation_p2).flatMap(_._2)
    val movesOf1_p2   = moves_p2.filter(of1(board))
    val movesOf23_p2  = moves_p2.filter(of23(board))

    "compute the correct number of moves for P2" in {
      moves_p2.size must_== 44

      lineMoves_p2.size must_== 34
      jumpMoves_p2.size must_== 10

      movesOf1_p2.size must_== 31
      movesOf23_p2.size must_== 13
    }
  }

  /*
   *     1 1 1 · ·
   *    · · 2 1 1 1
   *   · 1 1 2 2 2 2
   *  · · · 1 · · · ·
   * · · · 1 · · · · ·
   *  · · 1 · · · · ·
   *   · 2 · · · · ·
   *    2 2 2 2 · ·
   *     1 1 1 1 2
   */
  "custom start position" should {
    val fen        = format.FEN("SSSSs/ssss2/1s5/2S5/3S5/3S4/1SSssss/2sSSS/SSS2 5 5 b 0 0")
    val board      = Board(fen.pieces(Abalone.boardType), History(), Abalone)
    val situation  = Situation(board, P1)
    val validMoves = valid(situation)
    val lineMoves  = valid_line(situation).flatMap(_._2)
    val jumpMoves  = valid_jump(situation).flatMap(_._2)
    val moves      = validMoves.flatMap(_._2)
    val movesOf1   = moves.filter(of1(board))
    val movesOf23  = moves.filter(of23(board))

    "compute the correct number of moves" in {
      moves.size must_== 53

      lineMoves.size must_== 38
      jumpMoves.size must_== 15

      movesOf1.size must_== 27
      movesOf23.size must_== 26
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
   *     2 · · · ·
   *    1 2 · · · ·
   *   1 · 2 · · · ·
   *  1 · · 1 · · · ·
   * 2 · · · 1 · · · ·
   *  2 · · · 1 · · ·
   *   1 · · · 1 · ·
   *    1 2 2 2 2 2
   *     1 2 1 1 1
   */
  "special position to test some edge cases" should {
    val fen        = format.FEN("SsSSS/Ssssss/S3S2/s3S3/s3S4/S2S4/S1s4/Ss4/s4 5 5 b 0 0")
    val board      = Board(fen.pieces(Abalone.boardType), History(), Abalone)
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
      ukeys(validMoves, 4, 2).size must_== 7
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
   * · · · · · 1 1 1 2
   *  · · · · · · · ·
   *   · · · · · · ·
   *    · · · · · ·
   *     · · · · ·
   */
  "pushing out the only marble of P2 with a score of 0" should {
    val fenEndedGame = format.FEN("5/6/7/8/5SSSs/8/7/6/5 0 0 b 0 0")
    val board        = Board(fenEndedGame.pieces(Abalone.boardType), History(), Abalone)
    val game2        = next(new Game(Situation(board, P1)), 5, 4, 8, 4) // e6e9

    "increment the score of P1" in {
      board.history.score must_== Score()
      game2.situation.board.history.score must_== Score(p1 = 1)
    }

    "trigger a stalemate" in {
      game2.situation.end must_== true
      game2.situation.stalemate must_== true
      game2.situation.playable(true) must_== false
      game2.situation.status must_== Some(Status.Stalemate)
      game2.situation.winner must_== None
    }
  }

  /*
   *     · · · · ·
   *    · · · · · ·
   *   · · · · · · ·
   *  · · · · · · · ·
   * · · · · · 1 1 1 2
   *  · · · · · · · ·
   *   · · · · · · ·
   *    · · · · · ·
   *     · · · · ·
   */
  "pushing out the only marble of P2 with a score of 5" should {
    val fenEndedGame = format.FEN("5/6/7/8/5SSSs/8/7/6/5 5 0 b 0 0")
    val board        = Board(fenEndedGame.pieces(Abalone.boardType), History(score = Score(p1 = 5)), Abalone)
    val game         = new Game(Situation(board, P1))
    val game2        = next(game, 5, 4, 8, 4) // e6e9

    "increment the score of P1" in {
      board.history.score must_== Score(p1 = 5) // game.situation.board.history.score is Score(0,0)
      game2.situation.board.history.score must_== Score(p1 = 6)
    }

    "trigger a win condition defined by the variant, even though it detects the stalemate" in {
      game2.situation.end must_== true
      game2.situation.stalemate must_== true
      game2.situation.playable(true) must_== false
      game2.situation.status must_== Some(Status.VariantEnd)
      game2.situation.winner must_== Some(P1)
    }
  }

  /*
   *      · · · 2 2
   *     · · · 1 2 2
   *    · · · · 1 1 ·
   *   · · · · · · 1 ·
   *  · · · · 1 1 1 2 2
   *   · · · · · · · ·
   *    · · · · · · ·
   *     · · · · · ·
   *      2 · · ·
   */
  "P1 pushing out 6 marbles consecutively" should {
    val fenEndedGame = format.FEN("s4/6/7/8/4SSSss/6S1/4SS1/3Sss/3ss 0 0 b 0 0")
    val board        = Board(fenEndedGame.pieces(Abalone.boardType), History(), Abalone)
    val game         = new Game(Situation(board, P1))
    val game2        = next(game, 4, 4, 8, 4)   // e5e9
    /*
     *      · · · 2 2
     *     · · · 1 2 2
     *    · · · · 1 1 ·
     *   · · · · · · 1 ·
     *  · · · · - 1 1 1 2
     *   · · · · · · · ·
     *    · · · · · · ·
     *     · · · · · ·
     *      2 · · ·
     */
    val game3        = next(game2, 0, 0, 0, 1)  // a1b1
    /*
     *      · · · 2 2
     *     · · · 1 2 2
     *    · · · · 1 1 ·
     *   · · · · · · 1 ·
     *  · · · · · 1 1 1 2
     *   · · · · · · · ·
     *    · · · · · · ·
     *     2 · · · · ·
     *      - · · ·
     */
    val game4        = next(game3, 5, 4, 8, 4)  // e6e9
    /*
     *      · · · 2 2
     *     · · · 1 2 2
     *    · · · · 1 1 ·
     *   · · · · · · 1 ·
     *  · · · · · - 1 1 1
     *   · · · · · · · ·
     *    · · · · · · ·
     *     2 · · · · ·
     *      · · · ·
     */
    val game5        = next(game4, 0, 1, 0, 0)  // b1a1
    /*
     *      · · · 2 2
     *     · · · 1 2 2
     *    · · · · 1 1 ·
     *   · · · · · · 1 ·
     *  · · · · · · 1 1 1
     *   · · · · · · · ·
     *    · · · · · · ·
     *     - · · · · ·
     *      2 · · ·
     */
    val game6        = next(game5, 7, 4, 7, 8)  // e8i8
    /*
     *      · · · 2 2
     *     · · · 1 1 2
     *    · · · · 1 1 ·
     *   · · · · · · 1 ·
     *  · · · · · · 1 - 1
     *   · · · · · · · ·
     *    · · · · · · ·
     *     · · · · · ·
     *      2 · · ·
     */
    val game7        = next(game6, 0, 0, 0, 1)  // a1b1
    /*
     *      · · · 2 2
     *     · · · 1 1 2
     *    · · · · 1 1 ·
     *   · · · · · · 1 ·
     *  · · · · · · 1 · 1
     *   · · · · · · · ·
     *    · · · · · · ·
     *     2 · · · · ·
     *      - · · ·
     */
    val game8        = next(game7, 7, 5, 7, 8)  // f8i8
    /*
     *      · · · 1 2
     *     · · · 1 1 2
     *    · · · · 1 1 ·
     *   · · · · · · - ·
     *  · · · · · · 1 · 1
     *   · · · · · · · ·
     *    · · · · · · ·
     *     2 · · · · ·
     *      · · · ·
     */
    val game9        = next(game8, 0, 1, 0, 0)  // b1a1
    /*
     *      · · · 1 2
     *     · · · 1 1 2
     *    · · · · 1 1 ·
     *   · · · · · · · ·
     *  · · · · · · 1 · 1
     *   · · · · · · · ·
     *    · · · · · · ·
     *     - · · · · ·
     *      2 · · ·
     */
    val game10       = next(game9, 6, 7, 8, 7)  // h7h9
    /*
     *      · · · 1 2
     *     · · · - 1 1
     *    · · · · 1 1 ·
     *   · · · · · · · ·
     *  · · · · · · 1 · 1
     *   · · · · · · · ·
     *    · · · · · · ·
     *     · · · · · ·
     *      2 · · ·
     */
    val game11       = next(game10, 0, 0, 0, 1) // a1b1
    /*
     *      · · · 1 2
     *     · · · · 1 1
     *    · · · · 1 1 ·
     *   · · · · · · · ·
     *  · · · · · · 1 · 1
     *   · · · · · · · ·
     *    · · · · · · ·
     *     2 · · · · ·
     *      - · · ·
     */
    val game12       = next(game11, 6, 6, 8, 8) // g7i9
    /*
     *      · · · 1 1
     *     · · · · 1 1
     *    · · · · - 1 ·
     *   · · · · · · · ·
     *  · · · · · · 1 · 1
     *   · · · · · · · ·
     *    · · · · · · ·
     *     2 · · · · ·
     *      · · · ·
     */

    "should enter a non reversible state only when pushing out" in {
      game8.board.variant.isIrreversible(game9.situation.board.history.prevMove.get) must_== false
      game9.board.variant.isIrreversible(game10.situation.board.history.prevMove.get) must_== true
      game10.board.variant.isIrreversible(game11.situation.board.history.prevMove.get) must_== false
    }

    "increment the score of P1 each time he plays a move" in {
      board.history.score must_== Score()
      game2.situation.board.history.score must_== Score(p1 = 1)
      game4.situation.board.history.score must_== Score(p1 = 2)
      game6.situation.board.history.score must_== Score(p1 = 3)
      game8.situation.board.history.score must_== Score(p1 = 4)
      game10.situation.board.history.score must_== Score(p1 = 5)
      game12.situation.board.history.score must_== Score(p1 = 6)
    }

    "reset the halfMovesSinceLastCapture after P1 push out" in {
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
      game12.situation.stalemate must_== false
      game12.situation.playable(true) must_== false
      game12.situation.status must_== Some(Status.VariantEnd)
      game12.situation.winner must_== Some(P1)
    }
  }

  /*
   *      · · · 2 2
   *     · · · 1 2 2
   *    · · · · 1 1 ·
   *   · · · · · · 1 ·
   *  · · · · 1 1 1 2 2
   *   · · · · · · · ·
   *    · · · · · · ·
   *     · · · · · ·
   *      2 · · · ·
   */
  "P2 and P1 moving left right left right several times" should {
    val fenEndedGame = format.FEN("s4/6/7/8/4SSSss/6S1/4SS1/3Sss/3ss 0 0 b 0 0")
    val board        = Board(fenEndedGame.pieces(Abalone.boardType), History(), Abalone)
    val game         = new Game(Situation(board, P1))
    val game2        = next(game, 4, 4, 3, 4)  // e5e4
    val game3        = next(game2, 0, 0, 1, 0) // a1a2
    val game4        = next(game3, 3, 4, 4, 4) // e4e5
    val game5        = next(game4, 1, 0, 0, 0) // a2a1
    val game6        = next(game5, 4, 4, 3, 4) // e5e4
    val game7        = next(game6, 0, 0, 1, 0) // a1a2
    val game8        = next(game7, 3, 4, 4, 4) // e4e5
    val game9        = next(game8, 1, 0, 0, 0) // a2a1 <- DRAW !
    val game10       = next(game9, 4, 4, 3, 4) // e5e4 <- trying to play a neutral move

    "trigger a draw by repetition after seeing the same situation for the 3rd time, and no stalemate is detected" in {
      game8.situation.end must_== false
      game8.situation.playable(true) must_== true

      game9.situation.end must_== true
      game9.situation.stalemate must_== false
      game9.situation.playable(true) must_== false
      game9.situation.status must_== Some(Status.Draw)
      game9.situation.winner must_== None
    }

    "keep incrementing halfMovesSinceLastCapture after each player move" in {
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

  // TODO
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
  //    val game         = pply(board.variant)
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
  //    val game         = new Game(situation)
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
  // }
  //
  // class AbaloneVariantTestIsometry extends strategygames.chess.ChessTest {
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
