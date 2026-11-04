package strategygames.fiveinarow

import strategygames.MoveMetrics

import strategygames.fiveinarow.format.Uci

case class Offer10(
    situationBefore: Situation,
    after: Board,
    override val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def withMetrics(m: MoveMetrics): Offer10 = copy(metrics = m)

  def toUci: Uci.Offer10 = Uci.Offer10()

}
