package strategygames.dameo

import org.specs2.matcher.ValidatedMatchers

class DameoActorTest extends DameoTest with ValidatedMatchers {
  "man without captures" should {
    val fen = variant.Dameo.initialFen
    val board = Board(fen.pieces, variant.Dameo)
    val actors1 = Situation(board, P1).actors
    val actors2 = Situation(board, P2).actors

    "have 3 moves" in {
      val act1 = actors1.find(_.pos == Pos.C3).get
      val act2 = actors2.find(_.pos == Pos.C6).get
      act1.noncaptures.length must_== 3
      act2.noncaptures.length must_== 3

      act1.captures.length must_== 0
      act2.captures.length must_== 0
    }

    "have 2 moves at board edge" in {
      val act1l = actors1.find(_.pos == Pos.A1).get
      val act1r = actors1.find(_.pos == Pos.H1).get
      val act2l = actors2.find(_.pos == Pos.A8).get
      val act2r = actors2.find(_.pos == Pos.H8).get
      act1l.noncaptures.length must_== 2
      act1r.noncaptures.length must_== 2
      act2l.noncaptures.length must_== 2
      act2r.noncaptures.length must_== 2

      act1l.captures.length must_== 0
      act1r.captures.length must_== 0
      act2l.captures.length must_== 0
      act2r.captures.length must_== 0
    }

    "have linear moves" in {
      val act1 = actors1.find(_.pos == Pos.D1).get
      val act2 = actors2.find(_.pos == Pos.D8).get
      act1.noncaptures.length must_== 3
      act2.noncaptures.length must_== 3

      act1.captures.length must_== 0
      act2.captures.length must_== 0
    }

  }
}
/*
    "have linear moves blocked by board edge" in {

    }

    "have moves blocked by other pieces" in {
    }


    "have linear moves blocked by other pieces" in {

    }

    "have no linear move when blocked by a king" in {

    }
  }

  "man with captures" should {
    "have no non-capture moves" in {

    }

    "have only the maximal capture sequence" in {

    }

    "have 2 equally long capture sequences" in {

    }
  }

  "king without captures" should {
    "move in all directions" in {

    }

    "have n moves" in {

    }

    "not partake in linear movement" in {

    }
  }

  "man with captures" should {
    "have no non-capture moves" in {

    }

    "have only the maximal capture sequence" in {

    }

    "have 2 equally long capture sequences" in {

    }
  }

}
*/