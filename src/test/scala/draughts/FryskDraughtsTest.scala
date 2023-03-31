package strategygames.draughts
import strategygames.Player

import cats.data.Validated._
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class FryskDraughtsTest extends Specification with ValidatedMatchers {

  // https://playstrategy.dev/mgsSX9aA
  val moves = List(
    "4843",
    "0308",
    "4338",
    "0410",
    "3833",
    "0207",
    "3329",
    "0106",
    "2924",
    "1015",
    "2419",
    "0812",
    "5044",
    "1217",
    "4440",
    "0712",
    "4035",
    "1721",
    "4943",
    "0611",
    "4641",
    "1520",
    "1913",
    "1214",
    "4136",
    "0510",
    "4741",
    "2024",
    "3631",
    "1419",
    "4338",
    "2430",
    "3524",
    "2413",
    "1117",
    "3111",
    "1122",
    "1015",
    "1308",
    "1520",
    "0802",
    "2025",
    "0235",
    "2545",
    "4137",
    "4550",
    "3833",
    "5028",
    "2817",
    "1747"
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

  "Frysk Draughts game " should {
    "end in win for P2" in {
      val s = moves
        .foldLeft(Situation(variant.Frysk))((sit, uci) => move(sit, uci))

      s.winner must_== Some(Player.P2)
    }
  }

}
