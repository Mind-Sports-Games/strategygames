package strategygames.backgammon
import strategygames.MoveMetrics

import strategygames.backgammon.format.Uci

case class EndTurn(
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def situationAfter =
    Situation(finalizeAfter, !situationBefore.player)

  def finalizeAfter: Board = after updateHistory { h =>
    h.copy(
      lastTurn = h.currentTurn :+ toUci,
      currentTurn = List()
    )
  }

  def applyVariantEffect = this

  def player = situationBefore.player

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.EndTurn()

  override def toString = toUci.uci

}
