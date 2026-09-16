package strategygames.entropy
import strategygames.MoveMetrics

import strategygames.entropy.format.Uci

case class DrawCounter(
    role: Role,
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def player = situationBefore.player

  def situationAfter = Situation(finalizeAfter, player)

  def finalizeAfter: Board = after updateHistory { h =>
    h.copy(currentTurn = h.currentTurn :+ toUci)
  }

  lazy val lazySituationAfter = situationAfter

  def withHistory(h: History) = copy(after = after withHistory h)

  def withMetrics(m: MoveMetrics): DrawCounter = copy(metrics = m)

  def toUci: Uci.DrawCounter = Uci.DrawCounter(role)

  override def toString = toUci.uci

}
