package strategygames.go
import strategygames.MoveMetrics

import strategygames.go.format.Uci

case class Drop(
    piece: Piece,
    pos: Pos,
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after) {

  def situationAfter = Situation(finalizeAfter, !piece.player)

  def applyVariantEffect: Drop = before.variant addVariantEffect this

  def player = piece.player

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Drop(piece.role, pos)

  override def toString = toUci.uci

}
