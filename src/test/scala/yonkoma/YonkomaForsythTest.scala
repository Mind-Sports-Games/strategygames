package strategygames.yonkoma

import strategygames.yonkoma.format.{ FEN, Forsyth, Uci }
import strategygames.yonkoma.variant.{ Yonkoma10x10, Yonkoma12x12 }

class YonkomaForsythTest extends YonkomaTest {

  "the initial fen" should {

    "describe the 10x10 starting position with white to play" in {
      val fen = Yonkoma10x10.initialFen
      fen.value === "c8C/10/10/10/10/10/10/10/10/C8c w 1"
      fen.player === Some(P1)
      fen.fullMove === Some(1)
      fen.ply === Some(0)
      fen.initial must beTrue
      fen.pieces === Board.init(Yonkoma10x10).pieces
    }

    "describe the 12x12 starting position" in {
      val fen = Yonkoma12x12.initialFen
      fen.value === "c10C/12/12/12/12/12/12/12/12/12/12/C10c w 1"
      fen.initial must beTrue
      fen.pieces === Board.init(Yonkoma12x12).pieces
    }

    "round-trip through a situation unchanged" in {
      Forsyth
        .>>(Forsyth.<<@(Yonkoma10x10, Yonkoma10x10.initialFen).get)
        .value === Yonkoma10x10.initialFen.value
      Forsyth
        .>>(Forsyth.<<@(Yonkoma12x12, Yonkoma12x12.initialFen).get)
        .value === Yonkoma12x12.initialFen.value
    }

    "be built per variant from the same recipe" in {
      Forsyth.initial(Yonkoma10x10) === Yonkoma10x10.initialFen
      Forsyth.initial(Yonkoma12x12) === Yonkoma12x12.initialFen
      Forsyth.initial === Yonkoma10x10.initialFen
    }
  }

  "the board size" should {
    "come from the number of ranks in the fen" in {
      Yonkoma10x10.initialFen.boardSize === Some(Board.Dim10x10)
      Yonkoma12x12.initialFen.boardSize === Some(Board.Dim12x12)
      FEN("8/8/8/8/8/8/8/8 w 1").boardSize === None
    }

    "pick the variant when none is given" in {
      Forsyth.<<(Yonkoma12x12.initialFen).map(_.board.variant) === Some(Yonkoma12x12)
      Forsyth.<<(Yonkoma10x10.initialFen).map(_.board.variant) === Some(Yonkoma10x10)
      Forsyth.<<<(Yonkoma12x12.initialFen).map(_.situation.board.variant) === Some(Yonkoma12x12)
      Forsyth.<<(FEN("8/8/8/8/8/8/8/8 w 1")) === None
    }

    "refuse a fen for another board" in {
      Forsyth.<<@(Yonkoma10x10, Yonkoma12x12.initialFen) === None
      Forsyth.<<@(Yonkoma12x12, Yonkoma10x10.initialFen) === None
      Forsyth.<<<@(Yonkoma10x10, Yonkoma12x12.initialFen) === None
    }

    "refuse a rank that is wider or narrower than the board, or holds a letter it does not know" in {
      FEN("c9C/10/10/10/10/10/10/10/10/C8c w 1").boardSize === None
      FEN("c8C/11/10/10/10/10/10/10/10/C8c w 1").boardSize === None
      FEN("c7C/10/10/10/10/10/10/10/10/C8c w 1").boardSize === None
      FEN("c8X/10/10/10/10/10/10/10/10/C8c w 1").boardSize === None
      FEN("c8C/10/10/10/10/10/10/10//C8c w 1").boardSize === None
      FEN("c8C/10/10/10/10/10/10/10/10/C8c/ w 1").boardSize === None
      FEN("c8C/10/10/10/10/10/10/10/10/C08c w 1").boardSize === None
      Forsyth.<<@(Yonkoma10x10, FEN("c9C/10/10/10/10/10/10/10/10/C8c w 1")) === None
      Forsyth.<<@(Yonkoma10x10, FEN("c8C/11/10/10/10/10/10/10/10/C8c w 1")) === None
      Forsyth.<<(FEN("c8C/11/10/10/10/10/10/10/10/C8c w 1")) === None
    }
  }

  "a fen" should {
    val board = "c8C/10/10/10/10/10/10/10/10/C8c"

    "name the player to move with w or b and nothing else" in {
      FEN(s"$board w 1").player === Some(P1)
      FEN(s"$board b 1").player === Some(P2)
      FEN(s"$board x 1").player === None
      FEN(s"$board wb 1").player === None
      FEN(board).player === None
      Forsyth.<<@(Yonkoma10x10, FEN(s"$board x 1")) === None
      Forsyth.<<@(Yonkoma10x10, FEN(board)) === None
      Forsyth.<<<@(Yonkoma10x10, FEN(s"$board x 1")) === None
    }

    "hold two checkers a side, white on dark squares and black on light ones" in {
      // a third white checker on i2
      Forsyth.<<@(Yonkoma10x10, FEN("c8C/10/10/10/10/10/10/10/8C1/C8c w 1")) === None
      // one black checker only
      Forsyth.<<@(Yonkoma10x10, FEN("9C/10/10/10/10/10/10/10/10/C8c w 1")) === None
      // a white checker on the light square b1
      Forsyth.<<@(Yonkoma10x10, FEN("c8C/10/10/10/10/10/10/10/10/1C7c w 1")) === None
      // a black checker on the dark square a1
      Forsyth.<<@(Yonkoma10x10, FEN("c8C/10/10/10/10/10/10/10/10/c8C w 1")) === None
      // the four checkers anywhere legal are fine
      Forsyth.<<@(Yonkoma10x10, FEN("10/10/10/10/4cC4/4C5/10/10/10/9c b 2")).isDefined must beTrue
    }
  }

  "a fen with two-digit runs of empty squares" should {
    val fen = FEN("10/10/10/10/10/3cC5/3Cc5/10/10/10 b 7")

    "read the board back" in {
      val pieces = fen.pieces
      pieces.get(Pos.D5) === Some(Piece(P2, Checker))
      pieces.get(Pos.E5) === Some(Piece(P1, Checker))
      pieces.get(Pos.D4) === Some(Piece(P1, Checker))
      pieces.get(Pos.E4) === Some(Piece(P2, Checker))
      pieces.size === 4
    }

    "read the player and the move number" in {
      fen.player === Some(P2)
      fen.fullMove === Some(7)
      fen.ply === Some(13)
      fen.initial must beFalse
    }

    "round-trip unchanged" in {
      Forsyth.>>(Forsyth.<<<@(Yonkoma10x10, fen).get).value === fen.value
    }

    "write a run of eleven empty squares on the 12x12 board as 11" in {
      val fen12 = FEN("12/12/12/12/12/12/12/12/12/C11/c10C/11c w 3")
      fen12.pieces.get(Pos.A3) === Some(Piece(P1, Checker))
      fen12.pieces.get(Pos.L2) === Some(Piece(P1, Checker))
      fen12.pieces.get(Pos.A2) === Some(Piece(P2, Checker))
      fen12.pieces.get(Pos.L1) === Some(Piece(P2, Checker))
      Forsyth.>>(Forsyth.<<<@(Yonkoma12x12, fen12).get).value === fen12.value
    }

    "give an empty replay no turns" in {
      Replay(Game(Yonkoma10x10)).chronoActions === Nil
      Replay(Game(Yonkoma10x10)).chronoPlies === Nil
    }
  }

  "a game" should {
    "export its position and move number after a few plies" in {
      val game = playActionStrs("a1e5 a10e6 j10f6", Yonkoma10x10).toOption.get
      Forsyth.>>(game).value === "10/10/10/10/4cC4/4C5/10/10/10/9c b 2"
      Forsyth.>>(game).ply === Some(3)
    }

    "be rebuilt from a fen with the right ply count" in {
      val fen  = FEN("10/10/10/10/4cC4/4C5/10/10/10/9c b 2")
      val game = Game(Some(Yonkoma10x10), Some(fen))
      game.plies === 3
      game.turnCount === 3
      game.player === P2
      game.situation.board.pieces === fen.pieces
      game.situation.board.variant === Yonkoma10x10
    }
  }

  "uci" should {

    "read and write moves with one and two digit ranks" in {
      Uci("a1j10").map(_.uci) === Some("a1j10")
      Uci("j10a1").map(_.uci) === Some("j10a1")
      Uci("l12a1").map(_.uci) === Some("l12a1")
      Uci("a1b2").map(_.uci) === Some("a1b2")
      Uci.Move("b10c11").map(m => (m.orig, m.dest)) === Some((Pos.B10, Pos.C11))
    }

    "read and write a pass" in {
      Uci("pass").map(_.uci) === Some("pass")
      Uci.readList("a1e5 pass e5d4").map(_.map(_.uci)) === Some(List("a1e5", "pass", "e5d4"))
    }

    "give none for anything else" in {
      Uci("a1z9") === None
      Uci("a1") === None
      Uci("a1e5e6") === None
      Uci("m13a1") === None
      Uci("Pass") === None
      Uci("") === None
      Uci.readList("a1e5 oops") === None
    }

    "give every square a distinct piotr character" in {
      Pos.all.map(_.piotr).distinct.size === Pos.all.size
      Pos.all.forall(p => Pos.piotr(p.piotr) == Some(p)) must beTrue
    }

    "round-trip moves and passes through piotr" in {
      val ucis = Uci.readList("a1e5 pass j10f6 l12a1").get
      ucis.map(u => Uci.piotr(u.piotr).map(_.uci)) === ucis.map(u => Some(u.uci))
    }
  }
}
