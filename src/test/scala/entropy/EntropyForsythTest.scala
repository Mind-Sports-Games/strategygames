package strategygames.entropy

import org.specs2.matcher.ValidatedMatchers

import format.{ FEN, Forsyth }

class EntropyForsythTest extends EntropyTest with ValidatedMatchers {

  "the initial fen" should {
    "be an empty 7x7 board with an empty pocket, P1 to play round one" in {
      val fen = variant.Entropy.initialFen
      fen.value === "7/7/7/7/7/7/7[] w 0 0 1 1"
      fen.pieces must beEmpty
      fen.player === Some(P1)
      fen.round === 1
      fen.player1Score === 0
      fen.player2Score === 0
    }

    "round-trip through a situation unchanged" in {
      val fen = variant.Entropy.initialFen
      Forsyth.>>(Forsyth.<<(fen).get).value === fen.value
    }
  }

  "a fen with counters, a pocket and a score" should {
    val fen = FEN("7/7/7/7/7/2k4/RgR4[Y] b 12 7 2 34")

    "read the board back" in {
      val pieces = fen.pieces
      pieces.get(Pos.A1) === Some(Piece(P1, Red))
      // lowercase is a P2-owned counter
      pieces.get(Pos.B1) === Some(Piece(P2, Green))
      pieces.get(Pos.C1) === Some(Piece(P1, Red))
      pieces.get(Pos.C2) === Some(Piece(P2, Black))
      pieces.size === 4
    }

    "read the drawn counter out of the pocket" in {
      fen.pocketRoles === List((P1, Yellow))
    }

    "read the player, scores and round" in {
      fen.player === Some(P2)
      fen.player1Score === 12
      fen.player2Score === 7
      fen.round === 2
    }

    "round-trip unchanged" in {
      Forsyth.>>(Forsyth.<<<(fen).get).value === fen.value
    }
  }

  "the round field" should {
    "distinguish two empty boards that differ only in whose turn it is to be Chaos" in {
      val roundOne = Forsyth.<<(FEN("7/7/7/7/7/7/7[] w 0 0 1 1")).get
      val roundTwo = Forsyth.<<(FEN("7/7/7/7/7/7/7[] b 0 47 2 50")).get

      roundOne.board.chaosPlayer === P1
      roundOne.board.orderPlayer === P2
      roundTwo.board.chaosPlayer === P2
      roundTwo.board.orderPlayer === P1
    }
  }
}
