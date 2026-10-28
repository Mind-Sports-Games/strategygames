package strategygames.fiveinarow

import strategygames.MoveMetrics

import strategygames.fiveinarow.format.Uci

case class Swap2(
    situationBefore: Situation,
    after: Board,
    override val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def withMetrics(m: MoveMetrics): Swap2 = copy(metrics = m)

  def toUci: Uci.Swap2 = Uci.Swap2()

}
