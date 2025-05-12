package strategygames.dameo

import org.specs2.matcher.ValidatedMatchers

import strategygames.dameo.format.Uci
import strategygames.dameo.Pos
import strategygames.dameo.King

class DameoUciTest extends DameoTest with ValidatedMatchers {
  "Uci" should {
    "basic moves" in {
      Uci.Move(Pos.C3, Pos.C4).uci must_== "c3c4"
      Uci.Move(Pos.A1, Pos.D4).uci must_== "a1d4"

      Uci.Move.apply("c3c4") must_== Some(Uci.Move(Pos.C3, Pos.C4))
      Uci.Move.apply("a1d4") must_== Some(Uci.Move(Pos.A1, Pos.D4))
    }
    "capture move" in {
      Uci.Move(Pos.D4, Pos.F6).uci must_== "d4f6"
      Uci.Move.apply("d4f6") must_== Some(Uci.Move(Pos.D4, Pos.F6))
    }
    "king long leap capture move" in {
      Uci.Move(Pos.D1, Pos.D6).uci must_== "d1d6"
      Uci.Move.apply("d1d6") must_== Some(Uci.Move(Pos.D1, Pos.D6))
    }
    "promotion move" in {
      Uci.Move(Pos.E6, Pos.E8, promotion = Some(King)).uci must_== "e6e8K"
      Uci.Move.apply("e6e8K") must_== Some(Uci.Move(Pos.E6, Pos.E8, promotion = Some(King)))
    }
    "promotion and capture move" in {
      Uci.Move(Pos.E6, Pos.E8, promotion = Some(King)).uci must_== "e6e8K"
      Uci.Move.apply("e6e8K") must_== Some(
        Uci.Move(Pos.E6, Pos.E8, promotion = Some(King))
      )
    }
  }

  "piotr encoding" should {
    "be reflexive" in {
      val move = Uci.Move("b1a2").get
      Uci.Move.piotr(move.piotr) must_== Some(move)
    }
    "be reflexive including promotions" in {
      val move = Uci.Move("b1a2K").get
      Uci.Move.piotr(move.piotr) must_== Some(move)
    }
    "be reflexive including captures" in {
      val move = Uci.Move("c1a3").get
      Uci.Move.piotr(move.piotr) must_== Some(move)
    }
    "be reflexive including both captures and promotions" in {
      val move = Uci.Move("e3e1K").get
      Uci.Move.piotr(move.piotr) must_== Some(move)
    }
  }
}
