package strategygames.dameo

import org.specs2.matcher.ValidatedMatchers

import format.{ FEN, Forsyth }

class DameoForsythTest extends DameoTest with ValidatedMatchers {

  "situationFromFen" should {
    val fenFromVariant   = variant.Dameo.initialFen
    val situationFromFen = Forsyth.<<(fenFromVariant)

    "create the starting setup Situation" in {
      val board = Board(fenFromVariant.pieces, variant.Dameo)
      situationFromFen.get.board.pieces must_== board.pieces
      situationFromFen.get.player must_== P1
    }

    "export the starting Situation correctly" in {
      Forsyth.>>(situationFromFen.get).value must_==variant.Dameo.initialFen.value
    }
  }

  "situationPlusFromFen" should {
    val fenFromVariant   = variant.Dameo.initialFen
    val situationPlusFromFen = Forsyth.<<<(fenFromVariant)

    "create the starting setup as SituationPlus" in {
      val board = Board(fenFromVariant.pieces, variant.Dameo)
      situationPlusFromFen.get.situation.board.pieces must_== board.pieces
      situationPlusFromFen.get.situation.player must_== P1

      /* TODO check fullturncount, turncount and plies after some moves*/
    }
  }

  "fen with ghosts and kings" should {
    val fen = FEN("W:Wa5.k,b2.k,c4:Ba8.g,e8.p,f6,f7.k:H0:F1")

    "count the ghosts" in {
      Forsyth.countGhosts(fen) must_== 2
    }

    "count the kings" in {
      Forsyth.countKings(fen) must_== 3
    }

    "export the Situation correctly" in {
      val situation = Forsyth.<<(fen)
      Forsyth.>>(situation.get).value must_==fen.value
    }
  }
}
