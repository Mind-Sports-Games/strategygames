package strategygames.chess
package format.pgn

import strategygames.format.pgn._

class TagTest extends ChessTest {

  "Tags" should {
    // http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm#c8.1.1
    "be sorted" in {
      Tags(
        List(
          Tag(Tag.Site, "https://lichess.org/QuCzSfxw"),
          Tag(Tag.Round, "-"),
          Tag(Tag.Date, "2018.05.04"),
          Tag(Tag.P2, "penguingim1"),
          Tag(Tag.P1, "DrDrunkenstein"),
          Tag(Tag.Result, "1-0"),
          Tag(Tag.UTCDate, "2018.05.04"),
          Tag(Tag.UTCTime, "20:59:23"),
          Tag(Tag.P1Elo, "2870"),
          Tag(Tag.P2Elo, "2862"),
          Tag(Tag.P1RatingDiff, "+12"),
          Tag(Tag.P2RatingDiff, "-7"),
          Tag(Tag.Event, "Titled Arena 5")
        )
      ).sorted.value.map(_.name) must_== List(
        Tag.Event,
        Tag.Site,
        Tag.Date,
        Tag.Round,
        Tag.P1,
        Tag.P2,
        Tag.Result,
        Tag.UTCDate,
        Tag.UTCTime,
        Tag.P1Elo,
        Tag.P2Elo,
        Tag.P1RatingDiff,
        Tag.P2RatingDiff
      )
    }
  }
}
