package strategygames.dameo

import org.specs2.matcher.ValidatedMatchers

import strategygames.dameo.format.FEN
// import strategygames.{ Status }

class DameoFenTest extends DameoTest with ValidatedMatchers {
  "starting position FEN" should {
    val fen       = variant.Dameo.initialFen
    val pieces    = fen.pieces
    // val board     = Board(
    //   pieces,
    //   History(),
    //   variant.Dameo
    // )
    // val situation = Situation(board, P1)

    "have White starting the game" in {
      fen.player must_== Some(P1)
      fen.value.split(':').lift(0) must_== Some("W")

    }

    "have a total of 18 men per player" in {
      pieces.filter(p => p._2.player == P1).size must_== 18
      pieces.filter(p => p._2.player == P2).size must_== 18
    }

    "set both moves counters to expected initial value" in {
      fen.fullMove.get must_== 1
      fen.halfMoveClock.get must_== 0
    }

    "have the correct starting position" in {
      pieces.get(Pos.A1) must_== Some(Piece(P1, Man))
      pieces.get(Pos.B1) must_== Some(Piece(P1, Man))
      pieces.get(Pos.C1) must_== Some(Piece(P1, Man))
      pieces.get(Pos.D1) must_== Some(Piece(P1, Man))
      pieces.get(Pos.E1) must_== Some(Piece(P1, Man))
      pieces.get(Pos.F1) must_== Some(Piece(P1, Man))
      pieces.get(Pos.G1) must_== Some(Piece(P1, Man))
      pieces.get(Pos.H1) must_== Some(Piece(P1, Man))

      pieces.get(Pos.B2) must_== Some(Piece(P1, Man))
      pieces.get(Pos.C2) must_== Some(Piece(P1, Man))
      pieces.get(Pos.D2) must_== Some(Piece(P1, Man))
      pieces.get(Pos.E2) must_== Some(Piece(P1, Man))
      pieces.get(Pos.F2) must_== Some(Piece(P1, Man))
      pieces.get(Pos.G2) must_== Some(Piece(P1, Man))

      pieces.get(Pos.C3) must_== Some(Piece(P1, Man))
      pieces.get(Pos.D3) must_== Some(Piece(P1, Man))
      pieces.get(Pos.E3) must_== Some(Piece(P1, Man))
      pieces.get(Pos.F3) must_== Some(Piece(P1, Man))

      pieces.get(Pos.A8) must_== Some(Piece(P2, Man))
      pieces.get(Pos.B8) must_== Some(Piece(P2, Man))
      pieces.get(Pos.C8) must_== Some(Piece(P2, Man))
      pieces.get(Pos.D8) must_== Some(Piece(P2, Man))
      pieces.get(Pos.E8) must_== Some(Piece(P2, Man))
      pieces.get(Pos.F8) must_== Some(Piece(P2, Man))
      pieces.get(Pos.G8) must_== Some(Piece(P2, Man))
      pieces.get(Pos.H8) must_== Some(Piece(P2, Man))

      pieces.get(Pos.B7) must_== Some(Piece(P2, Man))
      pieces.get(Pos.C7) must_== Some(Piece(P2, Man))
      pieces.get(Pos.D7) must_== Some(Piece(P2, Man))
      pieces.get(Pos.E7) must_== Some(Piece(P2, Man))
      pieces.get(Pos.F7) must_== Some(Piece(P2, Man))
      pieces.get(Pos.G7) must_== Some(Piece(P2, Man))

      pieces.get(Pos.C6) must_== Some(Piece(P2, Man))
      pieces.get(Pos.D6) must_== Some(Piece(P2, Man))
      pieces.get(Pos.E6) must_== Some(Piece(P2, Man))
      pieces.get(Pos.F6) must_== Some(Piece(P2, Man))
    }

    "parse kings correctly" in {
      val rolesFen = FEN("W:Wa5,a7.k,c3,c4.k:Ba8.k,e8.k,f6:H0:F1")
      val pieces   = rolesFen.pieces
      pieces.get(Pos.A5) must_== Some(Piece(P1, Man))
      pieces.get(Pos.C3) must_== Some(Piece(P1, Man))
      pieces.get(Pos.F6) must_== Some(Piece(P2, Man))

      pieces.get(Pos.A7) must_== Some(Piece(P1, King))
      pieces.get(Pos.C4) must_== Some(Piece(P1, King))
      pieces.get(Pos.A8) must_== Some(Piece(P2, King))
      pieces.get(Pos.E8) must_== Some(Piece(P2, King))
    }

    "parse ghosts correctly" in {
      // If it's white's move then there can be only black ghosts
      val rolesFen = FEN("W:Wa5,c4:Ba8.g,e8.p,f6:H0:F1")
      val pieces   = rolesFen.pieces
      pieces.get(Pos.A5) must_== Some(Piece(P1, Man))
      pieces.get(Pos.C4) must_== Some(Piece(P1, Man))
      pieces.get(Pos.F6) must_== Some(Piece(P2, Man))

      pieces.get(Pos.A8) must_== Some(Piece(P2, GhostMan))
      pieces.get(Pos.E8) must_== Some(Piece(P2, GhostKing))
    }
  }
}
