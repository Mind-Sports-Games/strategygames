package strategygames.fairysf
import strategygames.fairysf.format.Uci
import strategygames.MoveMetrics
import strategygames.Player

abstract class Action(situationBefore: Situation, after: Board, metrics: MoveMetrics = MoveMetrics()) {
  def before = situationBefore.board
  def situationAfter: Situation

  def finalizeAfter: Board = after updateHistory { h =>
    h.copy(
      lastMove = Option(toUci)
    )
  }

  def player: Player

  def withMetrics(m: MoveMetrics): Action

  def toUci: Uci
}
