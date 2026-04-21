package strategygames.draughts
import strategygames.Player

import cats.data.Validated._
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class PoolDraughtsTests extends Specification with ValidatedMatchers {

  // https://playstrategy.org/UkIpe5ec
  val buggyMovesUkIpe5ec = List(
    "2419",
    "1115",
    "2824",
    "0811",
    "2318",
    "0913",
    "2623",
    "0609",
    "2217",
    "1522",
    "1726",
    "1115",
    "2522",
    "0914",
    "2420",
    "1524",
    "2318",
    "1423",
    "2619",
    "1928",
    "0509",
    "2824",
    "0914",
    "2419",
    "1417",
    "2114",
    "1017",
    "1726",
    "3122",
    "0408",
    "2723",
    "0811",
    "2925",
    "0106",
    "2521",
    "0610",
    "3227",
    "1115",
    "2218",
    "1522", // <- valid move
    "2225", // <- invalid move
    "2117",
    "1322",
    "3021",
    "1014",
    "2016",
    "1418",
    "2314",
    "2226",
    "1409",
    "2630",
    "0905",
    "0711",
    "1607",
    "3016",
    "0501",
    "0211",
    "2117",
    "1620",
    "0128",
    "2031",
    "3113",
    "2801",
    "1115",
    "0119",
    "1322",
    "1924",
    "0307",
    "2420",
    "0710",
    "2024",
    "1014",
    "2427",
    "1417",
    "2723",
    "1721",
    "2319",
    "2125",
    "1924",
    "2529",
    "2427",
    "1216",
    "2724",
    "1620",
    "2419",
    "2231",
    "1912",
    "2024",
    "1216",
    "2427",
    "1612",
    "2732",
    "1216",
    "3124",
    "1630",
    "3214",
    "3026",
    "2410",
    "2630",
    "2911",
    "3026",
    "1125",
    "2616",
    "2522",
    "1612",
    "2225",
    "1216",
    "2522",
    "1612",
    "2225",
    "1226",
    "2511",
    "2631",
    "1125",
    "3113",
    "2511",
    "1302",
    "1125"
  )

  val buggyMovesKingPromotion = List(
    "2217",
    "1014",
    "1710",
    "0615",
    "2419",
    "1524",
    "2819",
    "1115",
    "1910",
    "0714",
    "2622",
    "0206",
    "2724",
    "0307",
    "3127",
    "0610",
    "3228",
    "1015",
    "2419",
    "1524",
    "2431"
    // "3117"
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

  "Pool Draughts" should {
    "have valid captures" in {
      /*val s = buggyMovesUkIpe5ec
        .take(39)
        .foldLeft(Situation(variant.Pool))((sit, uci) => move(sit, uci))
      println(s.validMoves)

      println("---------------------------------------------------------")*/

      val s2 = buggyMovesUkIpe5ec
        .take(40)
        .foldLeft(Situation(variant.Pool))((sit, uci) => move(sit, uci))
      s2.player === Player.P1
      // println(s2.validMoves)

      // println("---------------------------------------------------------")

      /*val s3 = buggyMovesUkIpe5ec
        .take(40)
        .foldLeft(Situation(variant.Russian))((sit, uci) => move(sit, uci))
      println(s3.validMoves)*/

      val s5 = buggyMovesKingPromotion
        .foldLeft(Situation(variant.Pool))((sit, uci) => move(sit, uci))
      s5.player === Player.P1

    }
  }
}
