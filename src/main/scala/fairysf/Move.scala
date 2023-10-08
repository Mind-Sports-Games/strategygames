package strategygames.fairysf
import strategygames.MoveMetrics

import scala.annotation.nowarn
import strategygames.fairysf.format.Uci

case class Move(
    piece: Piece,
    orig: Pos,
    dest: Pos,
    situationBefore: Situation,
    after: Board,
    capture: Option[Pos],
    promotion: Option[PromotableRole],
    castle: Option[((Pos, Pos), (Pos, Pos))],
    enpassant: Boolean,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after) {

  def situationAfter =
    Situation(
      finalizeAfter,
      if (before.variant.switchPlayerAfterMove) !piece.player else piece.player
    )

  def applyVariantEffect: Move = before.variant addVariantEffect this

  // does this move capture an opponent piece?
  def captures = capture.isDefined

  def promotes = promotion.isDefined

  def castles = castle.isDefined

  // could get rid of this?
  def normalizeCastle =
    castle.fold(this) { case (_, (rookOrig, _)) =>
      copy(dest = rookOrig)
    }

  def player = piece.player

  def withPromotion(@nowarn op: Option[PromotableRole]): Option[Move] = None

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Move(orig, dest, promotion)

  override def toString = s"$piece ${toUci.uci}"
}
