package strategygames.chess
package format.pgn

import strategygames.format.pgn._

import cats.syntax.option._

class RenderTest extends ChessTest {

  private def glyphs(id: Int) =
    Glyph.find(id).fold(Glyphs.empty) { g =>
      Glyphs fromList List(g)
    }

  /*
[Event "WCh"]
[Site "Bonn GER"]
[Date "2008.10.14"]
[Round "1"]
[P1 "Kramnik,V"]
[P2 "Anand,V"]
[Result "1/2-1/2"]
[P1Elo "2772"]
[P2Elo "2783"]
[ECO "D14"]
[Annotator "IM Malcolm Pein"]
[EventDate "2008.10.14"]

{ It wasn't a riveting start but you don't get many risks taken in game one
when the score is still level. Kramnik asked a question, Anand answered
confidently }

1. d4 d5 2. c4 c6 3. Nc3 Nf6 4. cxd5 { The Exchange Slav, the sure way to
play with zero losing chances so an ideal choice for game one } 4... cxd5
5. Bf4 Nc6 6. e3 Bf5 7. Nf3 e6 { P2 cannot continue symmetrically for
too long of course but this is the most solid choice } 8. Qb3 Bb4 9. Bb5
O-O { P2 breaks the symmetry but this is still the main line of chess
opening theory } 10. Bxc6 (10. O-O Bxc3 11. Bxc6 Bxb2 12. Bxb7 Bxa1 13.
   */

  "PGN string output" should {
    "be correct when there are no move times" in {
      val pgn = Pgn(
        tags = Tags(
          List(
            Tag(_.P1, "Kramnik,V"),
            Tag(_.P2, "Anand,V"),
            Tag(_.ECO, "D14")
          )
        ),
        turns = List(
          Turn(
            number = 1,
            p1 = Move("d4").some,
            p2 = Move("d5").some
          ),
          Turn(
            number = 2,
            p1 = Move("c4", glyphs = glyphs(1)).some,
            p2 = Move("c6", glyphs = glyphs(2)).some
          ),
          Turn(
            number = 3,
            p1 = Move("Nc3", glyphs = glyphs(3)).some,
            p2 = Move("Nf6").some
          ),
          Turn(
            number = 4,
            p1 = Move(
              "cxd5",
              comments =
                "The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one" :: Nil
            ).some,
            p2 = Move("cxd5").some
          ),
          Turn(
            number = 5,
            p1 = Move("Bf4").some,
            p2 = Move("Nc6").some
          )
        )
      )
      pgn.toString must_== """[P1 "Kramnik,V"]
[P2 "Anand,V"]
[ECO "D14"]

1. d4 d5 2. c4! c6? 3. Nc3!! Nf6 4. cxd5 { The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one } 4... cxd5 5. Bf4 Nc6"""
    }
    "be correct when there are move times" in {
      val pgn = Pgn(
        tags = Tags(
          List(
            Tag(_.P1, "tsinnema"),
            Tag(_.P2, "stockfish"),
            Tag(_.TimeControl, "300"),
            Tag(_.ECO, "A00e")
          )
        ),
        turns = List(
          Turn(
            number = 1,
            p1 = Move("a4", secondsLeft = 298.some).some,
            p2 = Move("Nf6", secondsLeft = 299.some).some
          ),
          Turn(
            number = 2,
            p1 = Move("d4", secondsLeft = 295.some).some,
            p2 = Move("d5", secondsLeft = 298.some).some
          ),
          Turn(
            number = 3,
            p1 = Move("h4", secondsLeft = 292.some).some,
            p2 = Move("e6", secondsLeft = 297.some).some
          ),
          Turn(
            number = 4,
            p1 = Move(
              "Qd3",
              glyphs = glyphs(1),
              secondsLeft = 288.some,
              comments = "An invention of true genius." :: Nil
            ).some,
            p2 = Move("c5", secondsLeft = 296.some).some
          ),
          Turn(
            number = 5,
            p1 = Move("dxc5", secondsLeft = 258.some).some,
            p2 = Move("Bxc5", glyphs = glyphs(1), secondsLeft = 295.some).some
          )
        )
      )
      pgn.toString must_== """[P1 "tsinnema"]
[P2 "stockfish"]
[TimeControl "300"]
[ECO "A00e"]

1. a4 { [%clk 0:04:58] } 1... Nf6 { [%clk 0:04:59] } 2. d4 { [%clk 0:04:55] } 2... d5 { [%clk 0:04:58] } 3. h4 { [%clk 0:04:52] } 3... e6 { [%clk 0:04:57] } 4. Qd3! { An invention of true genius. } { [%clk 0:04:48] } 4... c5 { [%clk 0:04:56] } 5. dxc5 { [%clk 0:04:18] } 5... Bxc5! { [%clk 0:04:55] }"""
    }

    "be correct with NAGs" in {
      val pgn = Pgn(
        tags = Tags.empty,
        turns = List(
          Turn(
            number = 1,
            p1 = Move("d3", glyphs = glyphs(6)).some,
            p2 = Move("Nc6", glyphs = glyphs(10)).some
          ),
          Turn(
            number = 2,
            p1 = Move("Qd2").some,
            p2 = Move(
              "Nb4",
              glyphs = Glyphs(
                Glyph.MoveAssessment.blunder.some,
                Glyph.PositionAssessment.p1MuchBetter.some,
                List(Glyph.Observation.timeTrouble)
              )
            ).some
          ),
          Turn(
            number = 3,
            p1 = Move("Qxb4", glyphs = glyphs(7)).some,
            p2 = None
          )
        )
      )
      pgn.toString must_== """1. d3?! Nc6 $10 2. Qd2 Nb4?? $18 $138 3. Qxb4 $7"""
    }

    "be correct with variations" in {
      val pgn = Pgn(
        tags = Tags.empty,
        turns = List(
          Turn(
            number = 1,
            p1 = Move(
              "d4",
              variations = List(
                List(
                  Turn(
                    number = 1,
                    p1 = Move("e4").some,
                    p2 = None
                  )
                )
              )
            ).some,
            p2 = Move(
              "Nf6",
              variations = List(
                List(
                  Turn(
                    number = 1,
                    p1 = None,
                    p2 = Move("d5").some
                  )
                )
              )
            ).some
          )
        )
      )
      pgn.toString must_== """1. d4 (1. e4) 1... Nf6 (1... d5)"""
    }
    "handle Elon Musk-style baby names like [=0040.34h5a4] in tags" in {
      val pgn = Pgn(
        tags = Tags(
          List(
            Tag(_.P1, "tsinnema"),
            Tag(_.P2, "[=0040.34h5a4]"),
            Tag(_.TimeControl, "300"),
            Tag(_.ECO, "A00e")
          )
        ),
        turns = List(
          Turn(
            number = 1,
            p1 = Move("a4", secondsLeft = 298.some).some,
            p2 = Move("Nf6", secondsLeft = 299.some).some
          ),
          Turn(
            number = 2,
            p1 = Move("d4", secondsLeft = 295.some).some,
            p2 = Move("d5", secondsLeft = 298.some).some
          ),
          Turn(
            number = 3,
            p1 = Move("h4", secondsLeft = 292.some).some,
            p2 = Move("e6", secondsLeft = 297.some).some
          ),
          Turn(
            number = 4,
            p1 = Move(
              "Qd3",
              glyphs = glyphs(1),
              secondsLeft = 288.some,
              comments = "An invention of true genius." :: Nil
            ).some,
            p2 = Move("c5", secondsLeft = 296.some).some
          ),
          Turn(
            number = 5,
            p1 = Move("dxc5", secondsLeft = 258.some).some,
            p2 = Move("Bxc5", glyphs = glyphs(1), secondsLeft = 295.some).some
          )
        )
      )
      pgn.toString must_== """[P1 "tsinnema"]
[P2 "[=0040.34h5a4]"]
[TimeControl "300"]
[ECO "A00e"]

1. a4 { [%clk 0:04:58] } 1... Nf6 { [%clk 0:04:59] } 2. d4 { [%clk 0:04:55] } 2... d5 { [%clk 0:04:58] } 3. h4 { [%clk 0:04:52] } 3... e6 { [%clk 0:04:57] } 4. Qd3! { An invention of true genius. } { [%clk 0:04:48] } 4... c5 { [%clk 0:04:56] } 5. dxc5 { [%clk 0:04:18] } 5... Bxc5! { [%clk 0:04:55] }"""
    }
    "result only" in {
      val pgn = Pgn(
        tags = Tags(
          List(
            Tag(_.Result, "0-1")
          )
        ),
        turns = List()
      )
      pgn.toString must_== """[Result "0-1"]

0-1"""
    }
  }

  "initial comments" should {
    "empty" in {
      val pgn = Pgn(
        tags = Tags.empty,
        turns = List()
      )
      pgn.toString must_== """"""
    }
    "empty with initial comment" in {
      val pgn = Pgn(
        tags = Tags.empty,
        turns = List(),
        initial = Initial(List("Why hello there!"))
      )
      pgn.toString must_== """{ Why hello there! }"""
    }
    "empty with initial comments" in {
      val pgn = Pgn(
        tags = Tags.empty,
        turns = List(),
        initial = Initial(
          List(
            "Why hello there!",
            "The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one"
          )
        )
      )
      pgn.toString must_== """{ Why hello there! } { The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one }"""
    }
    "moves with initial comments" in {
      val pgn = Pgn(
        tags = Tags.empty,
        turns = List(
          Turn(
            number = 1,
            p1 = Move(
              "d4",
              variations = List(
                List(
                  Turn(
                    number = 1,
                    p1 = Move("e4").some,
                    p2 = None
                  )
                )
              )
            ).some,
            p2 = Move(
              "Nf6",
              variations = List(
                List(
                  Turn(
                    number = 1,
                    p1 = None,
                    p2 = Move("d5").some
                  )
                )
              )
            ).some
          )
        ),
        initial = Initial(
          List(
            "Why hello there!",
            "The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one"
          )
        )
      )
      pgn.toString must_== """{ Why hello there! } { The Exchange Slav, the sure way to play with zero losing chances so an ideal choice for game one }
1. d4 (1. e4) 1... Nf6 (1... d5)"""
    }
  }
}
