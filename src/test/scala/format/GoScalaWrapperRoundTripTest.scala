package strategygames.format

import org.specs2.mutable.Specification

import strategygames.{ ActionStrs, GameLogic }
import strategygames.go.variant.Go9x9Scala
import strategygames.variant.Variant

private object GoScalaWrapperRoundTripFixture {

  val lib: GameLogic   = GameLogic.Go()
  val variant: Variant = Variant.Go(Go9x9Scala)
  val gameFamily       = variant.gameFamily

  val actionStrs: ActionStrs = Vector(
    Vector("s@g3"),
    Vector("s@c7"),
    Vector("s@f2"),
    Vector("s@e5"),
    Vector("s@e1"),
    Vector("s@d4"),
    Vector("s@h4"),
    Vector("s@i1"),
    Vector("s@i5"),
    Vector("s@c5"),
    Vector("s@d5"),
    Vector("s@d6"),
    Vector("pass"),
    Vector("pass"),
    Vector("ss:i1")
  )

  val ucis: Option[List[Uci]] = Uci.readList(lib, gameFamily, actionStrs.flatten.mkString(" "))

  val replay: Option[strategygames.Replay] =
    ucis.flatMap(us => strategygames.Replay(lib, us, None, variant).toOption)

  val dumped: Option[ActionStrs] = UciDump(lib, actionStrs, None, variant).toOption

  val fastPathResult: Option[String] = GameToUciStrings(lib, actionStrs, None, variant).toOption

  val expectedJoin: String = actionStrs.map(_.mkString(",")).mkString(" ")
}

class GoScalaWrapperRoundTripTest extends Specification {

  import GoScalaWrapperRoundTripFixture._

  "the top-level wrapper for a new pure-scala go variant" should {

    "build a game from actionStrs via strategygames.Replay" in {
      ucis.map(_.size) must beSome(actionStrs.flatten.size)
      replay must beSome
    }

    "round trip the actionStrs through UciDump" in {
      dumped must beSome(actionStrs)
    }

    "leave the actionStrs untouched through the GameToUciStrings identity fast path" in {
      fastPathResult must beSome(expectedJoin)
    }
  }
}
