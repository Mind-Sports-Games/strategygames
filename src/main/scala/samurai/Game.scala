package strategygames.samurai
import strategygames.{ Clock, MoveMetrics, Player }

import cats.data.Validated

import strategygames.samurai.format.{ pgn, FEN }

case class Game(
    situation: Situation,
    actions: Vector[Vector[String]] = Vector(),
    clock: Option[Clock] = None,
    turns: Int = 0, // plies
    startedAtPly: Int = 0,
    startPlayer: Player = Player.P1
) {
  def apply(
      orig: Pos,
      dest: Pos,
      promotion: Option[PromotableRole] = None,
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, Move)] =
    situation.move(orig, dest, promotion).map(_ withMetrics metrics) map { move =>
      apply(move) -> move
    }

  def apply(move: Move): Game = {
    val newSituation = move.situationAfter

    copy(
      situation = newSituation,
      turns = turns + 1,
      actions = applyAction(move.toUci.uci),
      clock = applyClock(move.metrics, newSituation.status.isEmpty, newSituation.player != situation.player)
    )
  }

  private def applyClock(metrics: MoveMetrics, gameActive: Boolean, switchClock: Boolean) =
    clock.map { c =>
      {
        val newC = c.step(metrics, gameActive, switchClock)
        if (actions.size == 1 && switchClock) newC.start else newC
      }
    }

  private def applyAction(action: String): Vector[Vector[String]] =
    if (Player.fromTurnCount(actions.size + startPlayer.hashCode - 1) == situation.player)
      actions :+ Vector(action)
    else
      actions.updated(actions.size - 1, actions(actions.size - 1) :+ action)

  def player = situation.player

  def board = situation.board

  def halfMoveClock: Int = board.history.halfMoveClock

  /** Fullmove number: The number of the full move. It starts at 1, and is incremented after P2's move.
    */
  def fullMoveNumber: Int = 1 + turns / 2

  def withTurns(t: Int) = copy(turns = t)
}

object Game {
  def apply(variant: strategygames.samurai.variant.Variant): Game =
    new Game(Situation(Board init variant, P1))

  def apply(variantOption: Option[strategygames.samurai.variant.Variant], fen: Option[FEN]): Game = {
    val variant = variantOption | strategygames.samurai.variant.Variant.default
    val g       = apply(variant)
    fen
      .flatMap {
        format.Forsyth.<<<@(variant, _)
      }
      .fold(g) { parsed =>
        g.copy(
          situation = Situation(
            board = parsed.situation.board withVariant g.board.variant,
            player = parsed.situation.player
          ),
          turns = parsed.turns
        )
      }
  }
}
