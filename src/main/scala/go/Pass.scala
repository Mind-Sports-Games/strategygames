package strategygames.go
import strategygames.MoveMetrics

import strategygames.go.format.Uci

case class Pass(
    situationBefore: Situation,
    after: Board,
    autoEndTurn: Boolean,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore) {

  def situationAfter =
    Situation(finalizeAfter, if (autoEndTurn) !situationBefore.player else situationBefore.player)

  def finalizeAfter: Board = after updateHistory { h =>
    h.copy(
      lastTurn = if (autoEndTurn) h.currentTurn :+ toUci else h.lastTurn,
      currentTurn = if (autoEndTurn) List() else h.currentTurn :+ toUci
    )
  }

  def applyVariantEffect = this

  def player = situationBefore.player

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Pass()

  override def toString = toUci.uci

}
