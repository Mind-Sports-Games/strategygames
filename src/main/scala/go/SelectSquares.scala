package strategygames.go
import strategygames.MoveMetrics

import strategygames.go.format.Uci

case class SelectSquares(
    squares: List[Pos],
    situationBefore: Situation,
    after: Board,
    autoEndTurn: Boolean,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after) {

  def situationAfter =
    Situation(finalizeAfter, if (autoEndTurn) !situationBefore.player else situationBefore.player)

  def applyVariantEffect = this

  def player = situationBefore.player

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.SelectSquares(squares)

  override def toString = toUci.uci

}
