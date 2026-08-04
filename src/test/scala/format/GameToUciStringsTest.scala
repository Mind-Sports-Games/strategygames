package strategygames.format

import cats.data.Validated
import org.specs2.mutable.Specification

import strategygames.{ GameFamily, GameLogic }
import strategygames.variant.Variant

class GameToUciStringsTest extends Specification {

  private val goVariant = Variant.libStandard(GameLogic.Go())

  private def go(actionStrs: Seq[Seq[String]]): Validated[String, String] =
    GameToUciStrings(GameLogic.Go(), actionStrs, None, goVariant)

  private def join(actionStrs: Seq[Seq[String]]): String =
    actionStrs.map(_.mkString(",")).mkString(" ")

  private def fairySF(variant: Variant, actionStrs: Seq[Seq[String]]): Validated[String, String] =
    GameToUciStrings(GameLogic.FairySF(), actionStrs, None, variant)

  private val shogi    = Variant.FairySF(strategygames.fairysf.variant.Shogi)

  private val shogiGameWithPromotions: Seq[Seq[String]] =
    ("c3c4 b7b6 b2e5 b6b5 a3a4 b5b4 b3b4 b8b4 b1c3 b4b1+ P@b6 b1b6 c3d5 c9d8 h2c2 " +
      "P@b4 d5c7+ d8c7 e5c7+ b9c7 S@g5 b4b3+ c2i2 b3c3 P@b7 c3d3 i3i4 d3e3 i4i5 N@d3")
      .split(' ')
      .toVector
      .map(Vector(_))

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

  "GameToUciStrings for the shogi game family" should {

    "match UciDump for a game containing promotions" in {
      fairySF(shogi, shogiGameWithPromotions) must beEqualTo(
        UciDump(GameLogic.FairySF(), shogiGameWithPromotions, None, shogi).map(join)
      )
    }

    "produce a move list that Uci.readList can parse" in {
      val parsed = fairySF(shogi, shogiGameWithPromotions).toOption
        .flatMap(Uci.readList(GameLogic.FairySF(), GameFamily.Shogi(), _))
      parsed.map(_.size) must beSome(shogiGameWithPromotions.size)
    }

    "not hand back the stored actionStrs untouched" in {
      fairySF(shogi, shogiGameWithPromotions) must not(
        beEqualTo(Validated.valid(join(shogiGameWithPromotions)))
      )
    }

    "cover minishogi, since routing keys off the game family" in {
      strategygames.fairysf.variant.MiniShogi.gameFamily must beEqualTo(GameFamily.Shogi())
    }
  }
}
