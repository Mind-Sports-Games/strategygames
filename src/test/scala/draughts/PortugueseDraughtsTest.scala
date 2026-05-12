package strategygames.draughts
import strategygames.Player

import cats.data.Validated._
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class PortugueseDraughtsTest extends Specification with ValidatedMatchers {

  val testGameChoiceCapture = List(
    "2318",
    "0914",
    "1809",
    "0514",
    "2218",
    "1423",
    "2718",
    "0609",
    "1814",
    "1017",
    "2114", // double capture
    "1405", // double capture
    "0106",
    "0501",
    "0710",
    "2521",
    "1115",
    "2925",
    "0609",
    "2623",
    "1014",
    "0119",
    "0811",
    "2522",
    "1116",
    "2318",
    "1623", // only possible move in portuguese variant
    "3126", // move that doesn't capture backwards
    "0913",
    "1809",
    "0206",
    "0902",
    "0408",
    "2619",
    "0307",
    "0211", // forced king moves
    "1104", // forced king moves
    "1216",
    "1912",
    "1317",
    "2114"  // final move captured all of blacks pieces (P1 win)
  )

  val testGame2BlackWin = List(
    "2318",
    "1014",
    "2419",
    "1423",
    "2718",
    "1115",
    "1910",
    "0714",
    "1423",
    "2619",
    "0610",
    "2218",
    "1014",
    "3127",
    "1423",
    "2718",
    "0106",
    "1815",
    "0610",
    "1506",
    "0307",
    "0601",
    "0811",
    "2824",
    "0408",
    "2420",
    "0913",
    "2522",
    "0509",
    "2218",
    "0710",
    "0115",
    "0207",
    "1814",
    "1118",
    "1405",
    "1823",
    "0501",
    "1317",
    "2114",
    "0811",
    "1409",
    "1216",
    "1912",
    "0710",
    "0119",
    "1926",
    "1115",
    "3228",
    "1518",
    "2623",
    "1827",
    "3026",
    "2731",
    "2925",
    "3113",
    "1302",
    "2824",
    "0207",
    "2522",
    "0702",
    "2016",
    "0220",
    "2031",
    "3117",
    "1208",
    "1713",
    "0803",
    "1331",
    "0317",
    "3113"
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

  "Portuguese Draughts" should {
    "have one valid capture" in {
      val s = testGameChoiceCapture
        .take(26)
        .foldLeft(Situation(variant.Portuguese))((sit, uci) => move(sit, uci))

      s.validMoves.size === 1
      s.validMoves.keys === Set(Pos64(4, 4))
    }
  }

  "Portuguese Draughts" should {
    "not allow men capture backwards (therefore 7 pieces can move instead of 1)" in {
      val s = testGameChoiceCapture
        .take(27)
        .foldLeft(Situation(variant.Portuguese))((sit, uci) => move(sit, uci))
      println(s.validMoves)

      s.validMoves.size === 7
    }
  }

  "Portuguese Draughts" should {
    "win for white if captured all of black's pieces" in {
      val s = testGameChoiceCapture
        .foldLeft(Situation(variant.Portuguese))((sit, uci) => move(sit, uci))

      s.winner === Some(Player.P1)
    }
  }

  "Portuguese Draughts" should {
    "win for black if captures all of white's pieces" in {
      val s = testGame2BlackWin
        .foldLeft(Situation(variant.Portuguese))((sit, uci) => move(sit, uci))

      s.winner === Some(Player.P2)
    }
  }

}
