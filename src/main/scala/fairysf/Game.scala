package strategygames.fairysf
import strategygames.{ ClockBase, MoveMetrics, Player }

import cats.data.Validated

import strategygames.fairysf.format.{ FEN, Uci }

case class Game(
    situation: Situation,
    pgnMoves: Vector[String] = Vector(),
    clock: Option[ClockBase] = None,
    turns: Int = 0, // plies
    startedAtTurn: Int = 0
) {
  def apply(
      orig: Pos,
      dest: Pos,
      promotion: Option[PromotableRole] = None,
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, Move)] =
    situation.move(orig, dest, promotion).map(_.normalizeCastle withMetrics metrics) map { move =>
      apply(move) -> move
    }

  def withPlayer(c: Player) = copy(situation = situation.copy(player = c))

  def apply(move: Move): Game = {
    val newSituation = move.situationAfter

    copy(
      situation = newSituation,
      turns = turns + 1,
      pgnMoves = pgnMoves :+ move.toUci.uci,
      clock = applyClock(move.metrics, newSituation.status.isEmpty, newSituation.player != situation.player)
    )
  }

  def drop(
      role: Role,
      pos: Pos,
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, Drop)] =
    situation.drop(role, pos).map(_ withMetrics metrics) map { drop =>
      applyDrop(drop) -> drop
    }

  def applyDrop(drop: Drop): Game = {
    val newSituation = drop situationAfter

    copy(
      situation = newSituation,
      turns = turns + 1,
      pgnMoves = pgnMoves :+ drop.toUci.uci,
      clock = applyClock(drop.metrics, newSituation.status.isEmpty, newSituation.player != situation.player)
    )
  }

  def apply(uci: Uci.Move): Validated[String, (Game, Move)] = apply(uci.orig, uci.dest, uci.promotion)
  def apply(uci: Uci.Drop): Validated[String, (Game, Drop)] = drop(uci.role, uci.pos)
  def apply(uci: Uci): Validated[String, (Game, Action)]    = (uci match {
    case u: Uci.Move => apply(u)
    case u: Uci.Drop => apply(u)
  }) map { case (g, a) => g -> a }

  private def applyClock(metrics: MoveMetrics, gameActive: Boolean, switchClock: Boolean) =
    clock.map { c =>
      {
        val newC = c.step(metrics, gameActive, switchClock)
        if (turns - startedAtTurn == (2 * situation.board.variant.plysPerTurn - 1)) newC.start else newC
      }
    }

  def player = situation.player

  def board = situation.board

  def halfMoveClock: Int = board.history.halfMoveClock

  /** Fullmove number: The number of the full move. It starts at 1, and is incremented after P2's move.
    */
  def fullMoveNumber: Int = 1 + turns / 2

  def withTurns(t: Int) = copy(turns = t)
}

object Game {
  def apply(variant: strategygames.fairysf.variant.Variant): Game =
    new Game(Situation(Board init variant, P1))

  def apply(variantOption: Option[strategygames.fairysf.variant.Variant], fen: Option[FEN]): Game = {
    val variant = variantOption | strategygames.fairysf.variant.Variant.default
    val g       = apply(variant)
    fen
      .flatMap {
        format.Forsyth.<<<@(variant, _)
      }
      .fold(g) { parsed =>
        g.copy(
          situation = Situation(
            board = parsed.situation.board withVariant g.board.variant withPocketData {
              parsed.situation.board.pocketData orElse g.board.pocketData
            },
            player = parsed.situation.player
          ),
          turns = parsed.turns
        )
      }
  }
}
