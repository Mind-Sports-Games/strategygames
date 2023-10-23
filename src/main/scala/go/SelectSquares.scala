package strategygames.go
import strategygames.MoveMetrics

import strategygames.go.format.Uci

case class SelectSquares(
    squares: List[Pos],
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def situationAfter = Situation(finalizeAfter, !situationBefore.player)

  def applyVariantEffect = this

  def player = situationBefore.player

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.SelectSquares(squares)

  override def toString = toUci.uci

}
