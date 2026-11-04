package strategygames.fiveinarow

import org.specs2.mutable.Specification

import strategygames.Status
import strategygames.fiveinarow.variant.{ Gomoku, Pente, Renju, Variant }

class FiveInARowNoFiveLeftTest extends Specification with FiveInARowTestSupport {

  // runs of two in every direction, so every five points along a line hold both colours
  private def pattern(variant: Variant): Map[String, Role] =
    (for {
      x <- 0 until variant.boardSize.width
      y <- 0 until variant.boardSize.height
    } yield Pos.at(x, y).get.key -> (if ((x / 2 + y) % 2 == 0) BlackStone else WhiteStone)).toMap

  private def situationIn(variant: Variant, stones: Map[String, Role]): Situation =
    situationOf(boardIn(variant)(stones.toSeq*))

  private def blockedWithGaps(variant: Variant): Map[String, Role] =
    pattern(variant) -- List("a1", "h8", "o15")

  // b8 to g8 reads B B _ B B B between white stones: only black's overline fits through d8
  private def overlineOnly(variant: Variant): Map[String, Role] =
    pattern(variant) - "d8" ++ List(
      "a8" -> WhiteStone,
      "b8" -> BlackStone,
      "c8" -> BlackStone,
      "e8" -> BlackStone,
      "f8" -> BlackStone,
      "g8" -> BlackStone,
      "h8" -> WhiteStone
    )

  // b8 to f8 reads B B _ B B between white stones: only black's exact five fits through d8
  private def fiveOnly(variant: Variant): Map[String, Role] =
    pattern(variant) - "d8" ++ List(
      "a8" -> WhiteStone,
      "b8" -> BlackStone,
      "c8" -> BlackStone,
      "e8" -> BlackStone,
      "f8" -> BlackStone,
      "g8" -> WhiteStone
    )

  "a board with no five left to make" should {

    "end gomoku and renju as a draw before it fills" in {
      List(Gomoku, Renju).map { variant =>
        val s = situationIn(variant, blockedWithGaps(variant))
        s.board.isFull must beFalse
        s.end must beTrue
        s.winner must beNone
        s.status === Some(Status.Draw)
        s.board.variant.specialDraw(s) must beTrue
        s.dropsAsDrops must beEmpty
      }
    }

    "end the game on the drop that blocks the last five" in {
      val before = Game(situationIn(Gomoku, fiveOnly(Gomoku)))
      before.situation.end must beFalse
      val after  = play(before, "W@d8")
      after.situation.end must beTrue
      after.situation.status === Some(Status.Draw)
    }

    "be a draw in gomoku and renju when only an overline is left" in {
      List(Gomoku, Renju).map { variant =>
        val s = situationIn(variant, overlineOnly(variant))
        s.end must beTrue
        s.status === Some(Status.Draw)
      }
    }

    "play on in pente, where a capture can reopen a line" in {
      val s = situationIn(Pente, pattern(Pente) -- List("a1", "j10", "s19"))
      s.end must beFalse
      s.status must beNone
    }
  }

  "a board with a five still open" should {

    "play on while one colour can still fill a line" in {
      val s = situationIn(Gomoku, pattern(Gomoku) -- List("a1", "b1", "c1", "d1", "e1"))
      s.end must beFalse
      s.status must beNone
    }

    "play on in pente when only an overline is left" in {
      situationIn(Pente, overlineOnly(Pente)).end must beFalse
    }
  }

}
