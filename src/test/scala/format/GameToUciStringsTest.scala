package strategygames.format

import cats.data.Validated
import org.specs2.mutable.Specification

import strategygames.GameLogic
import strategygames.variant.Variant

class GameToUciStringsTest extends Specification {

  private val goVariant = Variant.libStandard(GameLogic.Go())

  private def go(actionStrs: Seq[Seq[String]]): Validated[String, String] =
    GameToUciStrings(GameLogic.Go(), actionStrs, None, goVariant)

  "GameToUciStrings identity fast path" should {

    "join an empty game to an empty string" in {
      go(Vector.empty) must beEqualTo(Validated.valid(""))
    }

    "return a single action unchanged" in {
      go(Vector(Vector("a1"))) must beEqualTo(Validated.valid("a1"))
    }

    "comma-join actions within a turn and space-join turns" in {
      go(Vector(Vector("a1", "b2"), Vector("c3"))) must beEqualTo(Validated.valid("a1,b2 c3"))
    }

    "preserve pass and selectSquares tokens verbatim" in {
      go(Vector(Vector("a1"), Vector("pass"), Vector("ss:b2c3"))) must beEqualTo(
        Validated.valid("a1 pass ss:b2c3")
      )
    }
  }
}
