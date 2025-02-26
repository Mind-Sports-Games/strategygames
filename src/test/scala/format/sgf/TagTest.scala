package strategygames
package format.sgf

import strategygames.format.sgf._
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

class TagTest extends Specification with ValidatedMatchers {

  "Tags" should {
    // https://www.red-bean.com/sgf/
    "be sorted" in {
      Tags(
        List(
          Tag(Tag.FF, "4"),
          Tag(Tag.CA, "UTF-8"),
          Tag(Tag.GM, "1"),
          Tag(Tag.PC, "PlayStrategy: https://lichess.org/QuCzSfxw"), // not actual game
          Tag(Tag.DT, "2018.05.04"),
          Tag(Tag.PB, "penguingim1"),
          Tag(Tag.PW, "DrDrunkenstein"),
          Tag(Tag.RE, "1-0"),
          Tag(Tag.WR, "2870"),
          Tag(Tag.BR, "2862"),
          Tag(Tag.EV, "Titled Arena 5")
        )
      ).sorted.value.map(_.name) must_== List(
        Tag.FF,
        Tag.GM,
        Tag.CA,
        Tag.DT,
        Tag.EV,
        Tag.PW,
        Tag.PB,
        Tag.RE,
        Tag.PC,
        Tag.WR,
        Tag.BR
      )
    }
  }
}
