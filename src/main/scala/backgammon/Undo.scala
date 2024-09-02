package strategygames.backgammon
import strategygames.MoveMetrics

import strategygames.backgammon.format.Uci

case class Undo(
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def player = situationBefore.player

  def situationAfter =
    Situation(finalizeAfter, player)

  def finalizeAfter: Board = after updateHistory { h =>
    h.copy(
      currentTurn = h.currentTurn.dropRight(1),
      forcedTurn = false,
      justUsedUndo = true
    )
  }

  def lazySituationAfter = situationAfter

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Undo()

  override def toString = toUci.uci

}
