package strategygames.fiveinarow

import org.specs2.mutable.Specification

import strategygames.Player
import strategygames.fiveinarow.format.{ FEN, Forsyth }
import strategygames.fiveinarow.variant.Gomoku

class FiveInARowForsythTest extends Specification with FiveInARowTestSupport {

  private def roundTrips(game: Game): Boolean =
    Forsyth.<<@(Gomoku, Forsyth.>>(game)).exists { read =>
      read.player == game.player &&
      read.board.pieces == game.situation.board.pieces &&
      read.blackSeat == game.situation.blackSeat &&
      read.openingStep == game.situation.openingStep
    }

  "the initial fen" should {

    "be an empty board, black next, P1 holding black, in the opening" in {
      Forsyth.initial.value === List.fill(15)("15").mkString("/") + " b 1 o 1"
      Forsyth.>>(start).value === Forsyth.initial.value
    }

    "read back to P1 to move" in {
      Forsyth.<<(Forsyth.initial).map(_.player) === Some(Player.P1)
      Forsyth.initial.player === Some(Player.P1)
    }
  }

  "a fen" should {

    "name stones by colour and record the state fields" in {
      fen(opened).split(' ').drop(1).mkString(" ") === "w 1 c 1"
      fen(opened).split(' ').head.split('/')(7) === "7BB6"
    }

    "record a swap in the black seat" in {
      fen(play(opened, "swap")).split(' ').drop(1).take(3).mkString(" ") === "w 2 -"
    }

    "record each step of swap2" in {
      val declared = play(opened, "swap2")
      fen(play(declared, "W@j8")).split(' ').drop(1).take(3).mkString(" ") === "b 1 s"
      fen(play(declared, "W@j8", "B@j9")).split(' ').drop(1).take(3).mkString(" ") === "w 2 f"
    }

    "round trip at every step of the opening" in {
      val games = List(
        start,
        play(start, "B@h8"),
        opened,
        play(opened, "swap"),
        play(opened, "W@j8"),
        play(opened, "swap2"),
        play(opened, "swap2", "W@j8"),
        play(opened, "swap2", "W@j8", "B@j9"),
        play(opened, "swap2", "W@j8", "B@j9", "swap"),
        play(opened, "swap2", "W@j8", "B@j9", "W@k8")
      )
      games.forall(roundTrips) must beTrue
    }

    "give each stone to the seat holding its colour" in {
      val read = Forsyth.<<(FEN(fen(play(opened, "swap")))).get
      read.board(Pos.fromKey("h8").get).map(_.player) === Some(Player.P2)
      read.board(Pos.fromKey("h9").get).map(_.player) === Some(Player.P1)
    }

    "read a two-digit run of empty points" in {
      val g = play(start, "B@o15")
      Forsyth.<<(FEN(fen(g))).get.board.pieces.keys.map(_.key) === Set("o15")
    }

    "refuse a next colour the stones contradict" in {
      Forsyth.<<(FEN(List.fill(15)("15").mkString("/") + " w 1 o 1")) must beNone
    }
  }
}
