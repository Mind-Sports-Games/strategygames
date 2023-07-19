package strategygames.go
import strategygames.MoveMetrics

import strategygames.go.format.Uci

case class Pass(
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def situationAfter = Situation(finalizeAfter, !situationBefore.player)

  def applyVariantEffect = this

  def player = situationBefore.player

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Pass()

  override def toString = toUci.uci

}
