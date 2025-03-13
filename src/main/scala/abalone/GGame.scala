package abalone

import abalone.format.UUci
import abalone.util.geometry.Cell
import cats.data.Validated
import strategygames.abalone.format.FEN
import strategygames.abalone.variant.Variant
import strategygames.abalone.{Game, P1, format}
import strategygames.{ClockBase, MoveMetrics, Player, VActionStrs}

case class GGame(
                  situation: SSituation,
                  actionStrs: VActionStrs = Vector(),
                  clock: Option[ClockBase] = None,
                  plies: Int = 0,
                  turnCount: Int = 0,
                  startedAtPly: Int = 0,
                  startedAtTurn: Int = 0
                ) {
  def apply(
             orig: Cell, dest: Cell,
             metrics: MoveMetrics = MoveMetrics()
           ): Validated[String, (GGame, MMove)] =
    situation.move(orig, dest).map(_ withMetrics metrics) map { move =>
      apply(move) -> move
    }

  def apply(move: MMove): GGame = {
    val newSituation = move.situationAfter
    val switchPlayer = situation.player != newSituation.player

    copy(
      situation = newSituation,
      plies = plies + 1,
      turnCount = turnCount + (if (switchPlayer) 1 else 0),
      actionStrs = applyActionStr(move.toUci.uci),
      clock = applyClock(move.metrics, newSituation.status.isEmpty, switchPlayer)
    )
  }

  def apply(uci: UUci.MMove): Validated[String, (GGame, MMove)] =
    apply(uci.orig, uci.dest)

  def apply(uci: UUci): Validated[String, (GGame, AAction)] = (uci match {
    case u: UUci.MMove => apply(u)
  }) map { case (g, a) => g -> a }

  private def applyClock(metrics: MoveMetrics, gameActive: Boolean, switchClock: Boolean) =
    clock.map { c => {
      val newC = c.step(metrics, gameActive, switchClock)
      if (turnCount - startedAtTurn == 1 && switchClock) newC.start else newC
    }
    }

  private def applyActionStr(actionStr: String): VActionStrs =
    if (hasJustSwitchedTurns || actionStrs.size == 0)
      actionStrs :+ Vector(actionStr)
    else
      actionStrs.updated(actionStrs.size - 1, actionStrs(actionStrs.size - 1) :+ actionStr)

  def hasJustSwitchedTurns: Boolean =
    player == Player.fromTurnCount(actionStrs.size + startedAtTurn)

  def player = situation.player

  def board = situation.board

  def halfMoveClock: Int = board.history.halfMoveClock

  // Aka Fullmove number (in Forsyth-Edwards Notation):
  // The number of the completed turns by each player ('full move')
  // It starts at 1, and is incremented after P2's move (turn)
  def fullTurnCount: Int = 1 + turnCount / 2

  def withTurnsAndPlies(p: Int, t: Int) = copy(plies = p, turnCount = t)
}

object GGame {
  def apply(variant: Variant): Game =
    new GGame(SSituation(BBoard init variant, P1))

  def apply(variantOption: Option[Variant], fen: Option[FEN]): Game = {
    val variant = variantOption | Variant.default
    val g = apply(variant)
    fen
      .flatMap {
        format.Forsyth.<<<@(variant, _)
      }
      .fold(g) { parsed =>
        g.copy(
          situation = SSituation(
            board = parsed.situation.board withVariant g.board.variant,
            player = parsed.situation.player
          ),
          plies = parsed.plies,
          turnCount = parsed.turnCount
        )
      }
  }
}