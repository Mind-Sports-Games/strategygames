package strategygames.yonkoma

import strategygames.yonkoma.format.{ FEN, Forsyth, Uci }
import strategygames.yonkoma.variant.{ Yonkoma10x10, Yonkoma12x12 }

class YonkomaForsythTest extends YonkomaTest {

  "the initial fen" should {

    "describe the 10x10 starting position with red to play" in {
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
  }

  "a fen with two-digit runs of empty squares" should {
    val fen = FEN("10/10/10/10/10/3Cc5/3cC5/10/10/10 b 7")

    "read the board back" in {
      val pieces = fen.pieces
      pieces.get(Pos.D5) === Some(Piece(P1, Checker))
      pieces.get(Pos.E5) === Some(Piece(P2, Checker))
      pieces.get(Pos.D4) === Some(Piece(P2, Checker))
      pieces.get(Pos.E4) === Some(Piece(P1, Checker))
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
      val fen12 = FEN("12/12/12/12/12/12/12/12/12/12/C11/11c w 3")
      fen12.pieces.get(Pos.A2) === Some(Piece(P1, Checker))
      fen12.pieces.get(Pos.L1) === Some(Piece(P2, Checker))
      Forsyth.>>(Forsyth.<<<@(Yonkoma12x12, fen12).get).value === fen12.value
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

    "give every square a distinct piotr character" in {
      Pos.all.map(_.piotr).distinct.size === Pos.all.size
      Pos.all.forall(p => Pos.piotr(p.piotr) == Some(p)) must beTrue
    }

    "round-trip moves and passes through piotr" in {
      val ucis = Uci.readList("a1e5 pass j10f6 l12a1").get
      Uci.readListPiotr(Uci.writeListPiotr(ucis)).map(_.map(_.uci)) === Some(ucis.map(_.uci))
    }
  }
}
