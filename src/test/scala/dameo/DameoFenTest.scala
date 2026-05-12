package strategygames.dameo

import org.specs2.matcher.ValidatedMatchers

import strategygames.dameo.format.FEN
import strategygames.Status

class DameoFenTest extends DameoTest with ValidatedMatchers {
  "starting position FEN" should {
    val fen       = variant.Dameo.initialFen
    val pieces    = fen.pieces
    val board     = Board(
      pieces,
      History(),
      variant.Dameo
    )
    val situation = Situation(board, P1)

    "have White starting the game" in {
      fen.player === Some(P1)
      fen.value.split(':').lift(0) === Some("W")

    }

    "have a total of 18 men per player" in {
      pieces.filter(p => p._2.player == P1).size === 18
      pieces.filter(p => p._2.player == P2).size === 18
    }

    "set both moves counters to expected initial value" in {
      fen.fullMove.get === 1
      fen.halfMoveClock.get === 0
    }

    "have the correct starting position" in {
      pieces.get(Pos.A1) === Some(Piece(P1, Man))
      pieces.get(Pos.B1) === Some(Piece(P1, Man))
      pieces.get(Pos.C1) === Some(Piece(P1, Man))
      pieces.get(Pos.D1) === Some(Piece(P1, Man))
      pieces.get(Pos.E1) === Some(Piece(P1, Man))
      pieces.get(Pos.F1) === Some(Piece(P1, Man))
      pieces.get(Pos.G1) === Some(Piece(P1, Man))
      pieces.get(Pos.H1) === Some(Piece(P1, Man))

      pieces.get(Pos.B2) === Some(Piece(P1, Man))
      pieces.get(Pos.C2) === Some(Piece(P1, Man))
      pieces.get(Pos.D2) === Some(Piece(P1, Man))
      pieces.get(Pos.E2) === Some(Piece(P1, Man))
      pieces.get(Pos.F2) === Some(Piece(P1, Man))
      pieces.get(Pos.G2) === Some(Piece(P1, Man))

      pieces.get(Pos.C3) === Some(Piece(P1, Man))
      pieces.get(Pos.D3) === Some(Piece(P1, Man))
      pieces.get(Pos.E3) === Some(Piece(P1, Man))
      pieces.get(Pos.F3) === Some(Piece(P1, Man))

      pieces.get(Pos.A8) === Some(Piece(P2, Man))
      pieces.get(Pos.B8) === Some(Piece(P2, Man))
      pieces.get(Pos.C8) === Some(Piece(P2, Man))
      pieces.get(Pos.D8) === Some(Piece(P2, Man))
      pieces.get(Pos.E8) === Some(Piece(P2, Man))
      pieces.get(Pos.F8) === Some(Piece(P2, Man))
      pieces.get(Pos.G8) === Some(Piece(P2, Man))
      pieces.get(Pos.H8) === Some(Piece(P2, Man))

      pieces.get(Pos.B7) === Some(Piece(P2, Man))
      pieces.get(Pos.C7) === Some(Piece(P2, Man))
      pieces.get(Pos.D7) === Some(Piece(P2, Man))
      pieces.get(Pos.E7) === Some(Piece(P2, Man))
      pieces.get(Pos.F7) === Some(Piece(P2, Man))
      pieces.get(Pos.G7) === Some(Piece(P2, Man))

      pieces.get(Pos.C6) === Some(Piece(P2, Man))
      pieces.get(Pos.D6) === Some(Piece(P2, Man))
      pieces.get(Pos.E6) === Some(Piece(P2, Man))
      pieces.get(Pos.F6) === Some(Piece(P2, Man))
    }

    "any man can make the first move" in {
      board.variant.validMoves(situation).size === 18
    }

    "corner piece has 2 starting moves" in {
      board.variant.validMoves(situation)(Pos.A1).size === 2
    }

    "side piece has 3 starting moves" in {
      board.variant.validMoves(situation)(Pos.B2).size === 3
    }

    "vanguard piece has 3 starting moves" in {
      board.variant.validMoves(situation)(Pos.E3).size === 3
    }
  }

  "FEN" should {
    "parse kings correctly" in {
      val rolesFen = FEN("W:Wa5,Ka7,c3,Kc4:BKa8,Ke8,f6:H0:F1")
      val pieces   = rolesFen.pieces
      pieces.get(Pos.A5) === Some(Piece(P1, Man))
      pieces.get(Pos.C3) === Some(Piece(P1, Man))
      pieces.get(Pos.F6) === Some(Piece(P2, Man))

      pieces.get(Pos.A7) === Some(Piece(P1, King))
      pieces.get(Pos.C4) === Some(Piece(P1, King))
      pieces.get(Pos.A8) === Some(Piece(P2, King))
      pieces.get(Pos.E8) === Some(Piece(P2, King))
    }

    "parse ghosts correctly" in {
      // If it's white's move then there can be only black ghosts
      val rolesFen = FEN("W:Wa5,c4:BGa8,Pe8,f6:H0:F1")
      val pieces   = rolesFen.pieces
      pieces.get(Pos.A5) === Some(Piece(P1, Man))
      pieces.get(Pos.C4) === Some(Piece(P1, Man))
      pieces.get(Pos.F6) === Some(Piece(P2, Man))

      pieces.get(Pos.A8) === Some(Piece(P2, GhostMan))
      pieces.get(Pos.E8) === Some(Piece(P2, GhostKing))
    }

    "parse active pieces correctly" in {
      // There will be only one active piece at a time
      FEN("W:Wa5,Ac4:BGa8,Pe8,f6:H0:F1").pieces.get(Pos.C4) === Some(Piece(P1, ActiveMan))
      FEN("W:Wa5,c4:BAa8,Pe8,f6:H0:F1").pieces.get(Pos.A8) === Some(Piece(P2, ActiveMan))
      FEN("W:Wa5,Bc4:BGa8,Pe8,f6:H0:F1").pieces.get(Pos.C4) === Some(Piece(P1, ActiveKing))
      FEN("W:Wa5,c4:BBa8,Pe8,f6:H0:F1").pieces.get(Pos.A8) === Some(Piece(P2, ActiveKing))
    }

    "parse empty boards" in {
      FEN("W:Wa5:Ba8:H0:F1").pieces === Map(Pos.A5 -> Piece(P1, Man), Pos.A8 -> Piece(P2, Man))
      FEN("W:Wa5:B:H0:F1").pieces === Map(Pos.A5 -> Piece(P1, Man))
      FEN("W:W:Ba8:H0:F1").pieces === Map(Pos.A8 -> Piece(P2, Man))
      FEN("W:W:B:H0:F1").pieces === Map()
    }
  }

  "Game just finished having" should {
    val fen       = FEN("W:WKb4:B:H0:F1")
    val pieces    = fen.pieces
    val board     = Board(
      pieces,
      History(),
      variant.Dameo
    )
    val situation = Situation(board, P2)

    "have ended and P1 is the winner" in {
      situation.end === true
      situation.playable(true) === false
      situation.status === Some(Status.VariantEnd)
      situation.winner === Some(P1)
    }
  }

  "Game having a player unable to move" should {
    val board     = Board(
      format.FEN("W:Wa1,a3,a4,b1,c5:Ba2:H0:F1").pieces,
      History(
      ),
      variant.Dameo
    )
    val situation = Situation(board, P2)

    "have ended and P1 is the winner" in {
      situation.end === true
      situation.playable(true) === false
      situation.status === Some(Status.VariantEnd)
      situation.winner === Some(P1)
    }
  }
}
