package strategygames.draughts
import strategygames.Player

import cats.data.Validated._
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class AntiDraughtsTest extends Specification with ValidatedMatchers {

  // https://playstrategy.dev/GZ27CiXa
  val moves = List(
    "3329",
    "1621",
    "2923",
    "1829",
    "3423",
    "1928",
    "3223",
    "2025",
    "4034",
    "1722",
    "3429",
    "1318",
    "3933",
    "2127",
    "3832",
    "2738",
    "4332",
    "1217",
    "2312",
    "1221",
    "0812",
    "2117",
    "1221",
    "3328",
    "2233",
    "3324",
    "3530",
    "2534",
    "3227",
    "2132",
    "3728",
    "3439",
    "4433",
    "0712",
    "3127",
    "1117",
    "3329",
    "2433",
    "3322",
    "2231",
    "3627",
    "1520",
    "2722",
    "1728",
    "4238",
    "2024",
    "4843",
    "2429",
    "4944",
    "1419",
    "3833",
    "2938",
    "3849",
    "4940",
    "4534",
    "1014",
    "3429",
    "2832",
    "5044",
    "3238",
    "4742",
    "3847",
    "4736",
    "4641",
    "3647",
    "2924",
    "1930",
    "4439",
    "4736",
    "3933",
    "1419",
    "3329",
    "3647",
    "2924",
    "4720"
  )

  def toMove(uci: String): format.Uci.Move = {
    format.Uci.Move(uci) match {
      case Some(m) => m
      case None    => sys.error("Unable to parse move")

    }
  }

  def move(s: Situation, uci: String): Situation =
    s.move(toMove(uci)) match {
      case Valid(move) => move.situationAfter
      case Invalid(e)  => sys.error(s"Move is invalid for position: ${e}")
    }

  "AntiDraughts game " should {
    "end in win for P1" in {
      val s = moves
        .foldLeft(Situation(variant.Antidraughts))((sit, uci) => move(sit, uci))

      s.winner === Some(Player.P1)
    }
  }

  "AntiDraughts game as International" should {
    "end in win for P2 instead" in {
      val s = moves
        .foldLeft(Situation(variant.Standard))((sit, uci) => move(sit, uci))

      s.winner === Some(Player.P2)
    }
  }

}
