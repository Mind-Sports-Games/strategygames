package strategygames.backgammon
import strategygames.Player

import org.specs2.matcher.ValidatedMatchers

import strategygames.backgammon.format.{ FEN, Forsyth }
import strategygames.backgammon.variant.Backgammon

class BackgammonFenTest extends BackgammonTest with ValidatedMatchers {

  "Backgammon stoneArray from initial fen" should {
    val fen    = Backgammon.initialFen
    val pieces = fen.pieces

    "be valid" in {
      fen.stoneArray must_== Array(
        "2s",
        "0",
        "0",
        "0",
        "0",
        "5S",
        "0",
        "3S",
        "0",
        "0",
        "0",
        "5s",
        "5S",
        "0",
        "0",
        "0",
        "3s",
        "0",
        "5s",
        "0",
        "0",
        "0",
        "0",
        "2S"
      )
    }
    "8 pieces" in {
      pieces.size must_== 8
    }
    "pieces at a1, e1, g1, l1, and rank 2" in {
      pieces.keys.toList.contains(Pos.A1) must_== true
      pieces.keys.toList.contains(Pos.E1) must_== true
      pieces.keys.toList.contains(Pos.G1) must_== true
      pieces.keys.toList.contains(Pos.L1) must_== true
      pieces.keys.toList.contains(Pos.A2) must_== true
      pieces.keys.toList.contains(Pos.E2) must_== true
      pieces.keys.toList.contains(Pos.G2) must_== true
      pieces.keys.toList.contains(Pos.L2) must_== true
    }
  }

  "Initial fen starting player" should {
    val fen = Backgammon.initialFen
    "Starting player is white/p1" in {
      fen.player must_== Some(Player.P1)
    }
  }

  "fen 6,2s,2s,2s,2s,2s,2s/6,5S,5[2S] - - b 8 3 10" should {
    val fen    = FEN("6,2s,2s,2s,2s,2s,2s/6,5S,5[2S] 5 6 b 8 3 10")
    val pieces = fen.pieces

    "Player turn is black/p2" in {
      fen.player must_== Some(Player.P2)
    }
    "stoneArray" in {
      fen.stoneArray must_== Array(
        "0",
        "0",
        "0",
        "0",
        "0",
        "5S",
        "0",
        "0",
        "0",
        "0",
        "0",
        "0",
        "0",
        "0",
        "0",
        "0",
        "0",
        "0",
        "2s",
        "2s",
        "2s",
        "2s",
        "2s",
        "2s"
      )
    }
    "7 pieces" in {
      pieces.size must_== 7
    }
    "pieces at g1, h1, i1, j1, k1, l1, g2" in {
      pieces.keys.toList.contains(Pos.G2) must_== true
      pieces.keys.toList.contains(Pos.H2) must_== true
      pieces.keys.toList.contains(Pos.I2) must_== true
      pieces.keys.toList.contains(Pos.J2) must_== true
      pieces.keys.toList.contains(Pos.K2) must_== true
      pieces.keys.toList.contains(Pos.L2) must_== true
      pieces.keys.toList.contains(Pos.G1) must_== true
    }
    "score" in {
      fen.player1Score must_== 8
      fen.player2Score must_== 3
    }
  }

  "gameplay" should {
    "match expected fen" in {
      val actionStrs = List(
        "1/5",
        "l2k2",
        "e1j1",
        "endturn",
        "2/4",
        "l1j1",
        "g2k2",
        "endturn",
        "2/1",
        "s@k2"
      )
      playActionStrs(actionStrs) must beValid.like { g =>
        Forsyth.>>(g) must_== FEN("5S,3,3s,1,4s,3,1S,1S/5s,3,2S,1,5S,2,1s,1,1s[1S,1s] 1 2 w 0 0 1")
      }
    }
  }

}
