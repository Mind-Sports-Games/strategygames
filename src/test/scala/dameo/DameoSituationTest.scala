package strategygames.dameo

import org.specs2.matcher.ValidatedMatchers
import strategygames.dameo.format.FEN

class DameoSituationTest extends DameoTest with ValidatedMatchers {
  "situation" should {
    "Report the captureLength for all pieces" in {
      // As used in lila/Event.scala
      val board     = Board(FEN("W:WKc6,d5,f5,f7,g8:Bd4,f4:H0:F1").pieces, variant.Dameo)
      val situation = Situation(board, P2)
      situation.allMovesCaptureLength must_== 3
    }

    "Report the captureLength for all pieces including kings" in {
      val board     = Board(FEN("W:WKc6,d5,f5,f7,g8:Bd4,Kf3:H0:F1").pieces, variant.Dameo)
      val situation = Situation(board, P2)
      situation.allMovesCaptureLength must_== 3
    }

    "Report the captureLength for a single (active) piece" in {
      val board     = Board(FEN("W:WKc6,d5,Gf5,Gf7,g8:Bd4,Af8:H0:F1").pieces, variant.Dameo)
      val situation = Situation(board, P2)
      situation.captureLengthFrom(Pos.F8) must_== Some(1)
    }

    "Report the captureLength for a single (active) king" in {
      val board     = Board(FEN("W:WKc6,c8,d5,Gf5,Gf7:Bd4,Bf8:H0:F1").pieces, variant.Dameo)
      val situation = Situation(board, P2)
      situation.captureLengthFrom(Pos.F8) must_== Some(1)
    }

  }
}
