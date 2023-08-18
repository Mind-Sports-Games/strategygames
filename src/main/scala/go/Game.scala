package strategygames.go
import strategygames.{ Clock, MoveMetrics }

import cats.data.Validated

import strategygames.go.format.{ pgn, FEN }

case class Game(
    situation: Situation,
    pgnMoves: Vector[String] = Vector(),
    clock: Option[Clock] = None,
    turns: Int = 0, // plies
    startedAtTurn: Int = 0
) {
  def apply(drop: Drop): Game = {
    val newSituation = drop.situationAfter

    copy(
      situation = newSituation,
      turns = turns + 1,
      pgnMoves = pgnMoves :+ drop.toUci.uci,
      clock = applyClock(drop.metrics, newSituation.status.isEmpty, newSituation.player != situation.player)
    )
  }

  def apply(pass: Pass): Game = {
    val newSituation = pass.situationAfter

    copy(
      situation = newSituation,
      turns = turns + 1,
      pgnMoves = pgnMoves :+ pass.toUci.uci,
      clock = applyClock(pass.metrics, newSituation.status.isEmpty, newSituation.player != situation.player)
    )
  }

  def apply(ss: SelectSquares): Game = {
    val newSituation = ss.situationAfter

    copy(
      situation = newSituation,
      turns = turns + 1,
      pgnMoves = pgnMoves :+ ss.toUci.uci,
      clock = applyClock(ss.metrics, newSituation.status.isEmpty, newSituation.player != situation.player)
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
    val newSituation = drop.situationAfter

    copy(
      situation = newSituation,
      turns = turns + 1,
      pgnMoves = pgnMoves :+ drop.toUci.uci,
      clock = applyClock(drop.metrics, newSituation.status.isEmpty, newSituation.player != situation.player)
    )
  }

  def pass(
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, Pass)] =
    situation.pass().map(_ withMetrics metrics) map { pass =>
      applyPass(pass) -> pass
    }

  def applyPass(pass: Pass): Game = {
    val newSituation = pass.situationAfter

    copy(
      situation = newSituation,
      turns = turns + 1,
      pgnMoves = pgnMoves :+ pass.toUci.uci,
      clock = applyClock(pass.metrics, newSituation.status.isEmpty, newSituation.player != situation.player)
    )
  }

  def selectSquares(
      squares: List[Pos],
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, SelectSquares)] =
    situation.selectSquares(squares).map(_ withMetrics metrics) map { ss =>
      applySelectSquares(ss) -> ss
    }

  def applySelectSquares(ss: SelectSquares): Game = {
    val newSituation = ss.situationAfter

    copy(
      situation = newSituation,
      turns = turns + 1,
      pgnMoves = pgnMoves :+ ss.toUci.uci,
      clock = applyClock(ss.metrics, newSituation.status.isEmpty, newSituation.player != situation.player)
    )
  }

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
  def apply(variant: strategygames.go.variant.Variant): Game =
    new Game(Situation(Board init variant, P1))

  def apply(variantOption: Option[strategygames.go.variant.Variant], fen: Option[FEN]): Game = {
    val variant = variantOption | strategygames.go.variant.Variant.default
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
