package strategygames.yonkoma

import strategygames.yonkoma.variant.{ Yonkoma10x10, Yonkoma12x12 }

class YonkomaRulesTest extends YonkomaTest {

  "the starting position" should {

    "put red on the dark corners and blue on the light corners of the 10x10 board" in {
      val board = Board.init(Yonkoma10x10)
      board.pieces.size === 4
      board(Pos.A1) === Some(Piece(P1, Checker))
      board(Pos.J10) === Some(Piece(P1, Checker))
      board(Pos.A10) === Some(Piece(P2, Checker))
      board(Pos.J1) === Some(Piece(P2, Checker))
      Pos.A1.isLight must beFalse
      Pos.J10.isLight must beFalse
      Pos.A10.isLight must beTrue
      Pos.J1.isLight must beTrue
    }

    "do the same on the 12x12 board" in {
      val board = Board.init(Yonkoma12x12)
      board.pieces.size === 4
      board(Pos.A1) === Some(Piece(P1, Checker))
      board(Pos.L12) === Some(Piece(P1, Checker))
      board(Pos.A12) === Some(Piece(P2, Checker))
      board(Pos.L1) === Some(Piece(P2, Checker))
    }

    "keep every checker inside its own board size" in {
      Board.init(Yonkoma10x10).pieces.keys.forall(Yonkoma10x10.boardSize.onBoard) must beTrue
      Yonkoma10x10.boardSize.onBoard(Pos.K1) must beFalse
      Yonkoma10x10.boardSize.onBoard(Pos.A11) must beFalse
      Yonkoma12x12.boardSize.onBoard(Pos.L12) must beTrue
    }

    "give red the first move" in {
      val game = Game.apply(Yonkoma10x10)
      game.situation.player === P1
      game.situation.moves must not(beEmpty)
      game.situation.canPass must beFalse
      game.situation.end must beFalse
    }

    "let red move either corner checker towards the blue ones" in {
      val s = Game.apply(Yonkoma10x10).situation
      // from a1 the only diagonal runs to j10, which red's own checker blocks
      destinationKeys(s, "a1") === List("b2", "c3", "d4", "e5", "f6", "g7", "h8", "i9")
      destinationKeys(s, "j10") === List("b2", "c3", "d4", "e5", "f6", "g7", "h8", "i9")
    }
  }

  "the legal move rule" should {

    // Figure 2 of the rule sheet: red on a1 and b2, blue on c4 and f1, red to move.
    // (The figure is drawn on a 6x6 board; the answer is the same on the 10x10 board,
    // since the squares beyond f6 on the long diagonal are all farther from the blues.)
    val figure2 = situation("10/10/10/10/10/10/2c7/10/1C8/C4c4", P1)

    "allow the moves the rule sheet lists from b2" in {
      destinationKeys(figure2, "b2") === List("c1", "c3", "d4", "e5")
    }

    "refuse a3, where the sum of distances grows" in {
      figure2.move(pos("b2"), pos("a3")).isInvalid must beTrue
    }

    "refuse f6, where the sum of distances grows more" in {
      figure2.move(pos("b2"), pos("f6")).isInvalid must beTrue
    }

    "refuse to pass through the partner on a1" in {
      figure2.move(pos("b2"), pos("a1")).isInvalid must beTrue
    }

    "accept a move that keeps the sum but moves away from the partner" in {
      // c1: distances 3 and 3 to the blues, still 6, but 2 from a1 instead of 1
      figure2.move(pos("b2"), pos("c1")).isValid must beTrue
    }

    "refuse a move that keeps the sum but comes closer to the partner" in {
      // red on h6 and a9, blue on h7 and a2: h6 to f8 keeps the sum at 8 (1+7 becomes
      // 2+6) but brings the checker from 7 squares to 5 from its partner
      val s = situation("10/C9/10/7c2/7C2/10/10/10/c9/10", P1)
      s.move(pos("h6"), pos("f8")).isInvalid must beTrue
      destinationKeys(s, "h6") must not(contain("f8"))
    }

    "stop a slide at the first occupied square" in {
      // red b2 may not jump the blue checker on d4 to reach e5
      val s = situation("10/10/10/10/10/10/3c6/10/1C8/C8c", P1)
      destinationKeys(s, "b2") must not(contain("e5"))
      destinationKeys(s, "b2") must not(contain("d4"))
    }

    "never offer a square off the board" in {
      val s = Game.apply(Yonkoma10x10).situation
      s.destinations.values.toList.flatMap(ds => ds).forall(Yonkoma10x10.boardSize.onBoard) must beTrue
    }
  }

  "the crosscut" should {

    "be recognised when the four checkers fill a 2x2 block on opposite diagonals" in {
      // red d4 and e5, blue d5 and e4
      val board = situation("10/10/10/10/10/3cC5/3Cc5/10/10/10", P1).board
      Yonkoma10x10.isCrosscut(board) must beTrue
    }

    "not be a crosscut when the colours share a diagonal" in {
      // red d4 and d5, blue e4 and e5: each colour on a file, not a diagonal
      val board = situation("10/10/10/10/10/3Cc5/3Cc5/10/10/10", P1).board
      Yonkoma10x10.isCrosscut(board) must beFalse
    }

    "not be a crosscut when the block is not 2x2" in {
      val board = situation("10/10/10/10/10/3c1C4/3C1c4/10/10/10", P1).board
      Yonkoma10x10.isCrosscut(board) must beFalse
    }

    "lose the game for the player who completes it" in {
      // as in Figure 3 of the rule sheet: red on d4 and f2, blue on d3 and e4; red moves
      // f2 to e3 and completes the block d3 e3 d4 e4, with the reds on one diagonal and
      // the blues on the other
      val before = situation("10/10/10/10/10/10/3Cc5/3c6/5C4/10", P1)
      before.end must beFalse
      val move   = before.move(pos("f2"), pos("e3"))
      move.isValid must beTrue
      val after  = move.toOption.get.situationAfter
      format.Forsyth.exportBoard(after.board) === "10/10/10/10/10/10/3Cc5/3cC5/10/10"
      after.player === P2
      after.end must beTrue
      after.winner === Some(P2)
      after.status === Some(strategygames.Status.VariantEnd)
      after.moves must beEmpty
    }

    "end the shortest possible game, which blue loses" in {
      val game = playActionStrs("a1e5 a10e6 j10f6 j1f5", Yonkoma10x10)
      game.isValid must beTrue
      val g    = game.toOption.get
      g.situation.end must beTrue
      g.situation.winner === Some(P1)
      g.situation.moves must beEmpty
      g.situation.canPass must beFalse
      g.plies === 4
      g.turnCount === 4
    }

    "refuse any further action once the game is over" in {
      val g = playActionStrs("a1e5 a10e6 j10f6 j1f5", Yonkoma10x10).toOption.get
      g.situation.move(pos("e5"), pos("d4")).isInvalid must beTrue
      g.situation.pass().isInvalid must beTrue
    }
  }

  "a player with no legal move" should {

    val stuck = playActionStrs("j10f6 j1e6 a1e5 a10d7 e5d6", Yonkoma10x10).toOption.get

    "have the turn skipped with a pass, and nothing else" in {
      stuck.situation.player === P2
      stuck.situation.moves must beEmpty
      stuck.situation.canPass must beTrue
      stuck.situation.end must beFalse
      stuck.situation.pass().isValid must beTrue
    }

    "hand the move back to the opponent after the pass" in {
      val passed = stuck.apply(stuck.situation.pass().toOption.get)
      passed.situation.player === P1
      passed.situation.moves must not(beEmpty)
      passed.plies === 6
      passed.turnCount === 6
      passed.actionStrs.lastOption === Some(Vector("pass"))
    }

    "not be able to pass while a move is available" in {
      Game.apply(Yonkoma10x10).situation.pass().isInvalid must beTrue
    }

    "play on to the end of the recorded game" in {
      val game = playActionStrs("j10f6 j1e6 a1e5 a10d7 e5d6 pass f6e7", Yonkoma10x10)
      game.isValid must beTrue
      game.toOption.get.situation.end must beTrue
      game.toOption.get.situation.winner === Some(P2)
    }
  }

  "the 12x12 board" should {

    "play its shortest game" in {
      val game = playActionStrs("l12g7 l1g6 a1f6 a12f7", Yonkoma12x12)
      game.isValid must beTrue
      game.toOption.get.situation.end must beTrue
      game.toOption.get.situation.winner === Some(P1)
    }

    "skip a turn when a player is stuck" in {
      val game = playActionStrs("l12g7 a12e8 a1f6 l1f7 g7f8 pass f6e7", Yonkoma12x12)
      game.isValid must beTrue
      game.toOption.get.situation.end must beTrue
      game.toOption.get.situation.winner === Some(P2)
    }

    "refuse the 10x10 opening move, which runs off its diagonal" in {
      // j10 is empty on the 12x12 board: the red checker is on l12
      Game.apply(Yonkoma12x12).situation.move(pos("j10"), pos("f6")).isInvalid must beTrue
    }
  }

  "board validity" should {
    "accept the starting position strictly" in {
      Board.init(Yonkoma10x10).valid(true) must beTrue
    }
    "reject a red checker on a light square" in {
      situation("c8C/10/10/10/10/10/10/10/10/1C7c", P1).board.valid(true) must beFalse
    }
    "reject a position with three checkers of one colour" in {
      situation("c8C/10/10/10/10/10/10/10/1C8/C8c", P1).board.valid(true) must beFalse
    }
  }
}
