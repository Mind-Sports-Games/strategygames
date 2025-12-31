package strategygames.draughts

import cats.data.Validated._
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class MultiMoveCaptureApi extends Specification with ValidatedMatchers {

  // https://playstrategy.dev/q3tX9jp6
  val moves = List(
    "3329",
    "1722",
    "3228",
    "2233",
    "3324",
    "3429",
    "2433",
    "3928",
    "1823",
    "3126",
    "2332",
    "3728",
    "1117",
    "3833",
    "2025",
    "3530",
    "2534",
    "4029",
    "0611",
    "2822",
    "1728",
    "2839",
    "4433",
    "1420",
    "2924",
    "2029",
    "2938",
    "4233",
    "1117",
    "3328",
    "1014",
    "4338",
    "0106",
    "3832",
    "0711",
    "3227",
    "1420",
    "3631",
    "2025",
    "4136",
    "1924",
    "2823",
    "1318",
    "2722",
    "1728",
    "1829", // bad bot
    "2621",
    "1627",
    "3122",
    "2233",
    "2938",
    "4843",
    "1520",
    "4332",
    "2430",
    "3228",
    "1117",
    "3631",
    "1722",
    "2817",
    "1221",
    "3126",
    "0813",
    "2617",
    "0208",
    "4641",
    "0410",
    "4136",
    "1318",
    "3631",
    "0913",
    "3126",
    "0309",
    "2621",
    "2024",
    "2116",
    "0611",
    "1706",
    "0914",
    "0601",
    "2429",
    "0123",
    "2340",
    "1319",
    "1611",
    "3035",
    "4001",
    "1420",
    "1107",
    "1923",
    "0702",
    "2024",
    "0129",
    "2915",
    "1504",
    "0812",
    "4540",
    "3544",
    "4940",
    "1217",
    "4035",
    "1722",
    "0431",
    "2530",
    "3524",
    "0510",
    "0219",
    "1015",
    "2420",
    "1524",
    "2413"
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

  "MultiMoveCaptures Draughts" should {
    "have one valid capture" in {
      val s = moves
        .take(45)
        .foldLeft(Situation(variant.Standard))((sit, uci) => move(sit, uci))

      s.validMoves.size === 1
    }
  }

}
