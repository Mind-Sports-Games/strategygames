package strategygames.fiveinarow

import strategygames.MoveMetrics

import strategygames.fiveinarow.format.Uci

case class Drop(
    piece: Piece,
    pos: Pos,
    situationBefore: Situation,
    after: Board,
    override val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def withMetrics(m: MoveMetrics): Drop = copy(metrics = m)

  def toUci: Uci.Drop = Uci.Drop(piece.role, pos)

}
