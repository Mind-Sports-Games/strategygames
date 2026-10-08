package strategygames.fiveinarow

import org.specs2.mutable.Specification

import strategygames.Player
import strategygames.fiveinarow.format.{ Forsyth, Uci }
import strategygames.fiveinarow.variant.Gomoku

trait FiveInARowTestSupport { self: Specification =>

  def start: Game = Game(Gomoku)

  def play(game: Game, ucis: String*): Game =
    ucis.foldLeft(game) { (g, str) =>
      val uci = Uci(str).getOrElse(sys.error(s"unreadable uci ${str}"))
      g.apply(uci).fold(err => sys.error(s"${str} refused: ${err}"), _._1)
    }

  def refused(game: Game, str: String): Boolean =
    Uci(str).exists(uci => game.apply(uci).isInvalid)

  def boardOf(stones: (String, Role)*): Board = {
    val empty = Board.init(Gomoku)
    stones
      .foldLeft(empty) { case (b, (key, role)) =>
        b.place(role, Pos.fromKey(key).get).getOrElse(sys.error(s"$key occupied"))
      }
      .withOpeningStep(OpeningStep.Play)
  }

  def situationOf(board: Board): Situation = Situation(board, board.playerToMove)

  def fen(game: Game): String = Forsyth.>>(game).value

  // P1 has just placed black, white, black, so P2 now chooses
  def opened: Game = play(start, "B@h8", "W@h9", "B@i8")

  def seat(game: Game): Player = game.situation.board.blackSeat

}
