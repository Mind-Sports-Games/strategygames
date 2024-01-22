package strategygames.backgammon
import strategygames.Player

import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class BackgammonFenTest extends Specification with ValidatedMatchers {

  "Backgammon stoneArray from initial fen" should {
    val fen = variant.Backgammon.initialFen

    "be valid" in {
      fen.stoneArray must_== Array(
        "2S",
        "0",
        "0",
        "0",
        "0",
        "5s",
        "0",
        "3s",
        "0",
        "0",
        "0",
        "5S",
        "5s",
        "0",
        "0",
        "0",
        "3S",
        "0",
        "5S",
        "0",
        "0",
        "0",
        "0",
        "2s"
      )
    }
    "8 pieces" in {
      fen.pieces.size must_== 8
    }
    "pieces at a1, e1, g1, l1, and rank 2" in {
      fen.pieces.keys.toList.contains(Pos.A1) must_== true
      fen.pieces.keys.toList.contains(Pos.E1) must_== true
      fen.pieces.keys.toList.contains(Pos.G1) must_== true
      fen.pieces.keys.toList.contains(Pos.L1) must_== true
      fen.pieces.keys.toList.contains(Pos.A2) must_== true
      fen.pieces.keys.toList.contains(Pos.E2) must_== true
      fen.pieces.keys.toList.contains(Pos.G2) must_== true
      fen.pieces.keys.toList.contains(Pos.L2) must_== true
    }
  }

  "Initial fen starting player" should {
    val fen = variant.Backgammon.initialFen
    "Starting player is white/p1" in {
      fen.player must_== Some(Player.P1)
    }
  }

  "fen 6,2s,2s,2s,2s,2s,2s/6,5S,5[SS] b - - 10" should {
    val fen = strategygames.backgammon.format.FEN("6,2s,2s,2s,2s,2s,2s/6,5S,5[SS] b - - 10")
    "Player turn is black/p2" in {
      fen.player must_== Some(Player.P2)
    }
    "stoneArray" in {
      fen.stoneArray must_== Array(
        "2s",
        "2s",
        "2s",
        "2s",
        "2s",
        "2s",
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
        "5S",
        "0",
        "0",
        "0",
        "0",
        "0"
      )
    }
    "7 pieces" in {
      fen.pieces.size must_== 7
    }
    "pieces at g1, h1, i1, j1, k1, l1, g2" in {
      fen.pieces.keys.toList.contains(Pos.G1) must_== true
      fen.pieces.keys.toList.contains(Pos.H1) must_== true
      fen.pieces.keys.toList.contains(Pos.I1) must_== true
      fen.pieces.keys.toList.contains(Pos.J1) must_== true
      fen.pieces.keys.toList.contains(Pos.K1) must_== true
      fen.pieces.keys.toList.contains(Pos.L1) must_== true
      fen.pieces.keys.toList.contains(Pos.G2) must_== true
    }
  }

}