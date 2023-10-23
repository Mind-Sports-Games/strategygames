package strategygames.samurai
import strategygames.{ Clock, MoveMetrics, Player }

import cats.data.Validated

import strategygames.samurai.format.FEN

case class Game(
    situation: Situation,
    actions: Vector[Vector[String]] = Vector(),
    clock: Option[Clock] = None,
    plies: Int = 0,
    turnCount: Int = 0,
    startedAtPlies: Int = 0,
    startedAtTurn: Int = 0
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
    val switchPlayer = situation.player != newSituation.player

    copy(
      situation = newSituation,
      plies = plies + 1,
      turnCount = turnCount + (if (switchPlayer) 1 else 0),
      actions = applyAction(move.toUci.uci, switchPlayer),
      clock = applyClock(move.metrics, newSituation.status.isEmpty, switchPlayer)
    )
  }

  private def applyClock(metrics: MoveMetrics, gameActive: Boolean, switchClock: Boolean) =
    clock.map { c =>
      {
        val newC = c.step(metrics, gameActive, switchClock)
        if (actions.size == 1 && switchClock) newC.start else newC
      }
    }

  private def applyAction(action: String, switchPlayer: Boolean): Vector[Vector[String]] =
    if (switchPlayer || actions.size == 0)
      actions :+ Vector(action)
    else
      actions.updated(actions.size - 1, actions(actions.size - 1) :+ action)

  def player = situation.player

  def board = situation.board

  def halfMoveClock: Int = board.history.halfMoveClock

  // Aka Fullmove number (in Forsyth-Edwards Notation):
  // The number of the completed turns by each player ('full move')
  // It starts at 1, and is incremented after P2's move (turn)
  def fullTurnCount: Int = 1 + turnCount / 2

  // def currentTurnCount: Int = turnCount + (if (actions.size > 0) 1 else 0)

  def withTurns(p: Int, t: Int) = copy(plies = p, turnCount = t)
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
          plies = parsed.plies,
          turnCount = parsed.turnCount
        )
      }
  }
}
