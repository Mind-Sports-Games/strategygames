package strategygames.abalone
import strategygames.MoveMetrics

import strategygames.abalone.format.Uci

case class Move(
    piece: Piece,
    orig: Pos,
    dest: Pos,
    situationBefore: Situation,
    after: Board,
    autoEndTurn: Boolean,
    capture: Option[Pos] = None, // @TODO: could just be the dest Pos in a capture move (because line moves are basically just marbles jumping over other ones)
    promotion: Option[PromotableRole] = None,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def situationAfter =
    Situation(finalizeAfter, !piece.player)

  def applyVariantEffect: Move = before.variant addVariantEffect this

  // does this move capture an opponent piece?
  def captures = capture.isDefined

  def promotes = promotion.isDefined

  def player = piece.player

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Move(orig, dest, promotion)

  override def toString = s"$piece ${toUci.uci}"
}
