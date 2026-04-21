package strategygames.draughts
import strategygames.Player
import strategygames.draughts.format.{ FEN, Forsyth }
import strategygames.draughts.variant.Variant

import cats.data.Validated._
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class EnglishDraughtsTest extends Specification with ValidatedMatchers {

  val testKingMoves = List(
    "2319",
    "1116",
    "2218",
    "1623",
    "2619",
    "1015",
    "1910",
    "0615",
    "1522",
    "2518",
    "0914",
    "1809",
    "0514",
    "2925",
    "1216",
    "3026",
    "0710",
    "2522",
    "1015",
    "2623",
    "0106",
    "2318",
    "1423",
    "2718",
    "1811",
    "0815",
    "3227",
    "0610",
    "2419",
    "1524",
    "2720",
    "2011",
    "1015",
    "2824",
    "0207",
    "1102",
    "0307",
    "0211",
    "1118",
    "0408",
    "1815",
    "0812",
    "2218",
    "1216",
    "2419",
    "1623",
    "3127",
    "2332",
    "2117",
    "3227",
    "1511"
  )

  val fullGameMoves = List(
    "1710",
    "0714",
    "2218",
    "0913",
    "1809",
    "0514",
    "2318",
    "1423",
    "2718",
    "1115",
    "1811",
    "0815",
    "2623",
    "1518",
    "2314",
    "0609",
    "1405",
    "0106",
    "0501",
    "0307",
    "0110",
    "1003",
    "0207",
    "0310",
    "0408",
    "1014",
    "1317",
    "1421",
    "0811",
    "2117",
    "1115",
    "1722",
    "1518",
    "2215"
  )

  val multiCaptureMoves = List(
    "2319",
    "1216",
    "1912",
    "1014",
    "2217",
    "0710",
    "1713",
    "1015",
    "2420",
    "0307",
    "1203"
  )

  private def toMove(uci: String): format.Uci.Move = {
    format.Uci.Move(uci) match {
      case Some(m) => m
      case None    => sys.error("Unable to parse move")

    }
  }

  private def move(s: Situation, uci: String): Situation =
    s.move(toMove(uci)) match {
      case Valid(move) => move.situationAfter
      case Invalid(e)  => sys.error(s"Move is invalid for position: ${e}")
    }

  private def fenToSituation(positionString: FEN, variant: Variant) = {
    val situation = Forsyth.<<@(variant, positionString)
    situation.getOrElse(Situation(variant))
  }

  "English Draughts Kings" should {
    "allow kings to move in both directions" in {
      val s = testKingMoves
        .foldLeft(Situation(variant.English))((sit, uci) => move(sit, uci))

      s.validMoves.size === 1
      s.validMoves(s.validMoves.keys.head).size === 4
    }
  }

  "English Draughts Full Game with starting fen" should {
    "end properly" in {
      val s = fullGameMoves
        .foldLeft(
          fenToSituation(
            FEN("W:W17,22,23,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,11,14"),
            variant.English
          )
        )((sit, uci) => move(sit, uci))

      s.checkMate === true
      s.winner === Some(Player.P1)
    }
  }

  "English Draughts multi capture" should {
    "should end on back row" in {
      val s = multiCaptureMoves
        .foldLeft(Situation(variant.English))((sit, uci) => move(sit, uci))

      s.validMoves.size === 6
    }
  }
}
