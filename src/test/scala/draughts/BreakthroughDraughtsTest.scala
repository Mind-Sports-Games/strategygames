package strategygames.draughts
import strategygames.Player

import cats.data.Validated._
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class BreakthroughDraughtsTest extends Specification with ValidatedMatchers {

  // https://playstrategy.dev/euSW6JXf
  val moves = List(
    "3228",
    "1923",
    "2819",
    "1324",
    "3126",
    "1721",
    "2617",
    "1122",
    "3429",
    "1823",
    "2918",
    "1827",
    "1217",
    "3631",
    "1721",
    "2722",
    "2127",
    "2218",
    "2736",
    "3731",
    "3627",
    "3832",
    "2738",
    "3829",
    "4136",
    "1621",
    "3631",
    "2126",
    "1812",
    "2637",
    "4231",
    "0817",
    "3934",
    "2933",
    "4842",
    "1721",
    "4641",
    "2126",
    "4338",
    "2637",
    "3748"
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

  "Breakthrough game " should {
    "end in win for P2" in {
      val s = moves
        .foldLeft(Situation(variant.Breakthrough))((sit, uci) => move(sit, uci))

      s.winner === Some(Player.P2)
    }
  }

}
