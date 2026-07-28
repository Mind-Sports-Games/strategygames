package strategygames.bench

import cats.data.Validated
import org.specs2.mutable.Specification

import strategygames.ActionStrs
import strategygames.format.{ GameToUciStrings, UciDump }

class GameToUciStringsBackgammonSpec extends Specification {

  private val fixtures: List[CorpusFixture] =
    CorpusGenerator.sizes.map(size => CorpusFixture.load("backgammon", size.key))

  private def joined(actionStrs: ActionStrs): String =
    actionStrs.map(_.mkString(",")).mkString(" ")

  private def baseline(fx: CorpusFixture): String =
    UciDump(fx.lib, fx.actionStrs, fx.initialFen, fx.variant)
      .map(joined)
      .getOrElse(sys.error(s"UciDump invalid for backgammon fixture"))

  private def stripAllCaptures(actionStrs: ActionStrs): ActionStrs =
    actionStrs.map(_.map(dropCaptureMarker))

  private def stripCapturesInEvenTurns(actionStrs: ActionStrs): ActionStrs =
    actionStrs.zipWithIndex.map {
      case (turn, index) if index % 2 == 0 => turn.map(dropCaptureMarker)
      case (turn, _)                       => turn
    }

  private def dropCaptureMarker(action: String): String =
    if (action.endsWith("x")) action.dropRight(1) else action

  private def matchesBaseline(transform: ActionStrs => ActionStrs) =
    fixtures
      .map(fx => GameToUciStrings(fx.lib, transform(fx.actionStrs), fx.initialFen, fx.variant) must beEqualTo(
        Validated.valid(baseline(fx))
      ))
      .reduce(_ and _)

  "GameToUciStrings backgammon fold" should {

    "match UciDump for in-memory (x-carrying) input" in {
      matchesBaseline(identity)
    }

    "re-derive x from DB-decoded (x-stripped) input" in {
      matchesBaseline(stripAllCaptures)
    }

    "re-derive x from mixed x / no-x input" in {
      matchesBaseline(stripCapturesInEvenTurns)
    }
  }
}
