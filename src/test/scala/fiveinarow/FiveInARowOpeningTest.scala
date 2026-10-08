package strategygames.fiveinarow

import org.specs2.mutable.Specification

import strategygames.Player

class FiveInARowOpeningTest extends Specification with FiveInARowTestSupport {

  "the opening" should {

    "begin with P1 to drop a black stone" in {
      start.player === Player.P1
      start.situation.openingStep === OpeningStep.Opening
      start.situation.nextColour === BlackStone
    }

    "make P1 drop black, white, black in that order" in {
      refused(start, "W@h8") must beTrue
      val one = play(start, "B@h8")
      one.player === Player.P1
      refused(one, "B@h9") must beTrue
      val two = play(one, "W@h9")
      two.player === Player.P1
      two.situation.nextColour === BlackStone
    }

    "give the white stone P1 drops to P2, who holds white" in {
      opened.situation.board(Pos.fromKey("h9").get).map(_.player) === Some(Player.P2)
    }

    "end P1's turn on the third stone, recorded as one turn" in {
      opened.player === Player.P2
      opened.situation.openingStep === OpeningStep.Choice
      opened.actionStrs === Vector(Vector("B@h8", "W@h9", "B@i8"))
    }

    "offer neither swap during P1's opening" in {
      start.situation.canSwap must beFalse
      start.situation.canSwap2 must beFalse
      refused(start, "swap") must beTrue
      refused(start, "swap2") must beTrue
    }
  }

  "P2's choice" should {

    "offer a drop, a swap and a swap2" in {
      opened.situation.canSwap must beTrue
      opened.situation.canSwap2 must beTrue
      opened.situation.nextColour === WhiteStone
    }

    "continue as white on a drop, leaving the opening for good" in {
      val g = play(opened, "W@j8")
      g.player === Player.P1
      seat(g) === Player.P1
      g.situation.openingStep === OpeningStep.Play
      g.situation.canSwap must beFalse
    }

    "on a swap, give P2 black and P1 white, without touching the board" in {
      val g = play(opened, "swap")
      seat(g) === Player.P2
      g.player === Player.P1
      g.situation.nextColour === WhiteStone
      g.situation.board.pieces.map { case (p, s) => p -> s.role } ===
        opened.situation.board.pieces.map { case (p, s) => p -> s.role }
      g.situation.board(Pos.fromKey("h8").get).map(_.player) === Some(Player.P2)
      g.situation.openingStep === OpeningStep.Play
      g.actionStrs.last === Vector("swap")
    }

    "after a swap, offer no further swap" in {
      val g = play(opened, "swap")
      g.situation.canSwap must beFalse
      refused(g, "swap") must beTrue
    }
  }

  "swap2" should {

    val declared = play(opened, "swap2")

    "keep P2 on the move to drop white, then black" in {
      declared.player === Player.P2
      declared.situation.openingStep === OpeningStep.Swap2Drops
      refused(declared, "B@j8") must beTrue
      val one = play(declared, "W@j8")
      one.player === Player.P2
      refused(one, "W@j9") must beTrue
    }

    "end on the second drop with the colours exchanged, P1 now holding white" in {
      val g = play(declared, "W@j8", "B@j9")
      seat(g) === Player.P2
      g.player === Player.P1
      g.situation.nextColour === WhiteStone
      g.situation.openingStep === OpeningStep.FinalChoice
      g.actionStrs === Vector(Vector("B@h8", "W@h9", "B@i8"), Vector("swap2", "W@j8", "B@j9"))
    }

    "leave P1 a drop or a swap, but not another swap2" in {
      val g = play(declared, "W@j8", "B@j9")
      g.situation.canSwap must beTrue
      g.situation.canSwap2 must beFalse
    }

    "let P1 continue with a white stone" in {
      val g = play(declared, "W@j8", "B@j9", "W@k8")
      g.player === Player.P2
      seat(g) === Player.P2
      g.situation.nextColour === BlackStone
      g.situation.openingStep === OpeningStep.Play
    }

    "let P1 swap back, so that P2 plays white" in {
      val g = play(declared, "W@j8", "B@j9", "swap")
      seat(g) === Player.P1
      g.player === Player.P2
      g.situation.nextColour === WhiteStone
      g.situation.openingStep === OpeningStep.Play
    }
  }

  "ordinary play" should {

    "alternate one stone a turn" in {
      val g = play(opened, "W@j8", "B@a1", "W@a2")
      g.actionStrs.drop(1).map(_.size) === Vector(1, 1, 1)
      g.player === Player.P1
    }

    "refuse an occupied point" in {
      refused(play(opened, "W@j8"), "B@h8") must beTrue
    }
  }
}
