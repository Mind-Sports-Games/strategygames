package strategygames.fiveinarow

import strategygames.MoveMetrics

import strategygames.fiveinarow.format.Uci

case class Swap(
    situationBefore: Situation,
    after: Board,
    override val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def withMetrics(m: MoveMetrics): Swap = copy(metrics = m)

  def toUci: Uci.Swap = Uci.Swap()

}
