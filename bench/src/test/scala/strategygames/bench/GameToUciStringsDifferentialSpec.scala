package strategygames.bench

import cats.data.Validated
import org.specs2.mutable.Specification

import strategygames.{ ActionStrs, GameLogic }
import strategygames.format.{ GameToUciStrings, UciDump }

class GameToUciStringsDifferentialSpec extends Specification {

  private val identityLibs: Set[GameLogic] =
    Set(GameLogic.Go(), GameLogic.Samurai(), GameLogic.Togyzkumalak(), GameLogic.Abalone(), GameLogic.FairySF())

  private def joined(actionStrs: ActionStrs): String =
    actionStrs.map(_.mkString(",")).mkString(" ")

  "GameToUciStrings" should {

    "match UciDump joined output for every committed fixture" in {
      val checks = for {
        family <- CorpusGenerator.families
        size   <- CorpusGenerator.sizes
      } yield {
        val fx         = CorpusFixture.load(family.key, size.key)
        val viaUciDump = UciDump(fx.lib, fx.actionStrs, fx.initialFen, fx.variant).map(joined)
        val viaNew     = GameToUciStrings(fx.lib, fx.actionStrs, fx.initialFen, fx.variant)
        viaNew must beEqualTo(viaUciDump)
      }
      checks.reduce(_ and _)
    }

    "match UciDump joined output for every non-default variant fixture" in {
      val diverging = CorpusGenerator.variantFixtures.filterNot { vf =>
        val fx         = CorpusFixture.load(vf.key, "long")
        val viaUciDump = UciDump(fx.lib, fx.actionStrs, fx.initialFen, fx.variant).map(joined)
        val viaNew     = GameToUciStrings(fx.lib, fx.actionStrs, fx.initialFen, fx.variant)
        viaNew == viaUciDump
      }.map(_.key)
      diverging must beEqualTo(List.empty[String])
    }

    "prove identity families' stored actionStrs equal UciDump output" in {
      val checks = for {
        family <- CorpusGenerator.families.filter(f => identityLibs.contains(f.lib))
        size   <- CorpusGenerator.sizes
      } yield {
        val fx         = CorpusFixture.load(family.key, size.key)
        val viaUciDump = UciDump(fx.lib, fx.actionStrs, fx.initialFen, fx.variant).map(joined)
        viaUciDump must beEqualTo(Validated.valid(joined(fx.actionStrs)))
      }
      checks.reduce(_ and _)
    }
  }
}
