package strategygames.fiveinarow

import org.specs2.mutable.Specification

import strategygames.{ Player, Status }

class FiveInARowEndingTest extends Specification with FiveInARowTestSupport {

  private def line(keys: String*)(role: Role): Seq[(String, Role)] = keys.map(_ -> role)

  "five in a row" should {

    "win along a row" in {
      val s = situationOf(boardOf(line("a1", "b1", "c1", "d1", "e1")(BlackStone)*))
      s.end must beTrue
      s.winner === Some(Player.P1)
      s.status === Some(Status.VariantEnd)
    }

    "win along a column" in {
      situationOf(boardOf(line("c3", "c4", "c5", "c6", "c7")(WhiteStone)*)).winner === Some(Player.P2)
    }

    "win along a rising diagonal" in {
      situationOf(boardOf(line("k11", "l12", "m13", "n14", "o15")(BlackStone)*)).winner === Some(Player.P1)
    }

    "win along a falling diagonal" in {
      situationOf(boardOf(line("a15", "b14", "c13", "d12", "e11")(WhiteStone)*)).winner === Some(Player.P2)
    }

    "count an overline as a win in gomoku" in {
      situationOf(boardOf(line("d8", "e8", "f8", "g8", "h8", "i8")(BlackStone)*)).winner === Some(Player.P1)
    }

    "not count four, or five broken by a gap" in {
      situationOf(boardOf(line("a1", "b1", "c1", "d1")(BlackStone)*)).end must beFalse
      situationOf(boardOf(line("a1", "b1", "c1", "d1", "f1")(BlackStone)*)).end must beFalse
    }

    "not join stones across the edge of the board" in {
      situationOf(boardOf(line("l1", "m1", "n1", "o1", "a2")(BlackStone)*)).end must beFalse
    }

    "go to whichever seat holds the colour after a swap" in {
      val swapped = boardOf(line("a1", "b1", "c1", "d1", "e1")(BlackStone)*).swapColours
      situationOf(swapped).winner === Some(Player.P2)
    }

    "end a game played to it, and offer nothing further" in {
      val g = play(opened, "W@a1", "B@g8", "W@a2", "B@j8", "W@a3", "B@k8")
      g.situation.end must beTrue
      g.situation.winner === Some(Player.P1)
      g.situation.dropsAsDrops must beEmpty
      refused(g, "W@a4") must beTrue
    }
  }

  "a full board without five" should {

    // runs of two in every direction, so no colour ever makes five
    val stones = for {
      x <- 0 until 15
      y <- 0 until 15
    } yield Pos.at(x, y).get.key -> (if ((x / 2 + y) % 2 == 0) BlackStone else WhiteStone)

    "be a draw" in {
      val s = situationOf(boardOf(stones*))
      s.board.isFull must beTrue
      s.end must beTrue
      s.winner must beNone
      s.board.variant.specialDraw(s) must beTrue
    }
  }
}
