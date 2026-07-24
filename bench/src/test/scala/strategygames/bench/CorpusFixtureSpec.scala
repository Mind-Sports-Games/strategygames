package strategygames.bench

import org.specs2.mutable.Specification

class CorpusFixtureSpec extends Specification {

  "committed corpus fixtures" should {

    "load, parse and round-trip render for every committed fixture" in {
      val checks = for {
        family <- CorpusGenerator.families
        size   <- CorpusGenerator.sizes
      } yield {
        val canonical = CorpusFixture.render(CorpusFixture.load(family.key, size.key))
        CorpusFixture.render(CorpusFixture.parse(canonical)) must beEqualTo(canonical)
      }
      checks.reduce(_ and _)
    }
  }

  "CorpusFixture.parse" should {

    "reject an unknown variant key" in {
      val corrupt = "family=chess\ngameLogic=0\nvariant=notarealvariant\ninitialFen=\nturns=0\n"
      CorpusFixture.parse(corrupt) must throwA[RuntimeException]
    }

    "reject a header line with no '='" in {
      val corrupt = "family=chess\ngameLogic 0\nvariant=standard\ninitialFen=\nturns=0\n"
      CorpusFixture.parse(corrupt) must throwA[RuntimeException]
    }
  }
}
