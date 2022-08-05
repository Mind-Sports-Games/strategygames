package strategygames.draughts
import strategygames.Player

import cats.data.Validated._
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class FrisianDraughtsTest extends Specification with ValidatedMatchers {

  // https://playstrategy.dev/GE7kFwa3
  val moves = List(
    "3329",
    "1721",
    "2923",
    "1333",
    "3829",
    "1217",
    "3227",
    "2132",
    "4222",
    "2213",
    "1324",
    "1621",
    "3430",
    "1434",
    "3423",
    "3933",
    "2127",
    "3122",
    "2224",
    "2029",
    "2938",
    "4332",
    "1520",
    "3024",
    "2029",
    "4439",
    "1116",
    "3919",
    "0929",
    "3228",
    "2927",
    "4742",
    "2747",
    "4738",
    "4828",
    "1621",
    "3631",
    "0611",
    "3126",
    "0409",
    "2822",
    "1728",
    "2617",
    "1706",
    "2833",
    "4034",
    "0813",
    "0608",
    "0819",
    "0929",
    "2940",
    "4534",
    "3432",
    "0308",
    "3228",
    "0207",
    "2823",
    "0813",
    "2303",
    "0106",
    "3530",
    "1014",
    "0325",
    "0510",
    "2503",
    "1015",
    "3025",
    "1535",
    "5045",
    "0712",
    "0326",
    "0611",
    "4525",
    "1116",
    "2606"
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

  "Frisian Draughts game " should {
    "end in win for P1" in {
      val s = moves
        .foldLeft(Situation(variant.Frisian))((sit, uci) => move(sit, uci))

      s.winner must_== Some(Player.P1)
    }
  }

}
