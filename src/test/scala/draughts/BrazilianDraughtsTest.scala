package strategygames.draughts
import strategygames.Player

import cats.data.Validated._
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class BrazilianDraughtsTest extends Specification with ValidatedMatchers {

  // https://playstrategy.dev/5rh3LplU
  val moves = List(
    "2318",
    "1116",
    "2420",
    "1014",
    "2011",
    "0815",
    "1811",
    "0716",
    "2217",
    "1620",
    "1710",
    "0615",
    "2724",
    "2027",
    "3124",
    "0307",
    "2420",
    "0106",
    "2522",
    "1518",
    "2215",
    "0408",
    "1510",
    "0615",
    "2623",
    "0711",
    "2319",
    "1524",
    "2027",
    "0914",
    "2117",
    "1421",
    "2925",
    "0207",
    "2723",
    "1216",
    "2824",
    "1620",
    "2318",
    "2027",
    "3223",
    "0509",
    "2319",
    "0812",
    "2522",
    "1216",
    "1912",
    "2125",
    "3021",
    "0914",
    "1809",
    "1115",
    "2218",
    "1522",
    "2117",
    "2213",
    "1306",
    "1208",
    "0710",
    "0803",
    "0609",
    "0314",
    "1405"
  )

  // midmulticapture
  val midMultiCaptureMoves = List(
    "2218",
    "1115",
    "1811",
    "0815",
    "2117",
    "0711",
    "2521",
    "1116",
    "2925",
    "0207",
    "2622",
    "0308",
    "2420",
    "1014",
    "1710"
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

  "Brazilian Draughts game " should {
    "end in win for P1" in {
      val s = moves
        .foldLeft(Situation(variant.Brazilian))((sit, uci) => move(sit, uci))

      s.winner === Some(Player.P1)
    }
  }

  "Partial Brazilian Draughts game " should {
    "have valid moves mid multicapture" in {
      val s = midMultiCaptureMoves
        .foldLeft(Situation(variant.Brazilian))((sit, uci) => move(sit, uci))

      s.validMoves.size !== 0
    }
  }

}
