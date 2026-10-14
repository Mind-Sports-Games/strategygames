package strategygames.fiveinarow

import strategygames.MoveMetrics

import strategygames.fiveinarow.format.Uci

case class SelectSquares(
    squares: List[Pos],
    situationBefore: Situation,
    after: Board,
    override val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def withMetrics(m: MoveMetrics): SelectSquares = copy(metrics = m)

  def toUci: Uci.SelectSquares = Uci.SelectSquares(squares)

}
