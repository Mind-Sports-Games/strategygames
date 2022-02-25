package strategygames.fairysf
import strategygames.MoveMetrics

import strategygames.fairysf.format.Uci

case class Drop(
    piece: Piece,
    pos: Pos,
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) {

  private def before = situationBefore.board

  def situationAfter = Situation(finalizeAfter, !piece.player)

  def finalizeAfter: Board = after

  def player = piece.player

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Drop(piece.role, pos)

  override def toString = toUci.uci

}
