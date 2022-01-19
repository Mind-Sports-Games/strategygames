package strategygames.fairysf
import strategygames.MoveMetrics

import cats.syntax.option.none

import strategygames.fairysf.format.Uci
import cats.syntax.option._

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
) {
  def before = situationBefore.board

  def situationAfter = Situation(finalizeAfter, !piece.color)

  def withHistory(h: History) = copy(after = after withHistory h)

  def finalizeAfter: Board = after.variant.finalizeBoard( 
    after updateHistory { h =>
        h.copy(
          lastMove = Option(toUci)
        )
    },
    toUci,
    none
  )

  def applyVariantEffect: Move = before.variant addVariantEffect this

  // does this move capture an opponent piece?
  def captures = capture.isDefined

  def promotes = promotion.isDefined

  def castles = castle.isDefined

  //could get rid of this?
  def normalizeCastle =
    castle.fold(this) { case (_, (rookOrig, _)) =>
      copy(dest = rookOrig)
    }

  def color = piece.color

  def withPromotion(op: Option[PromotableRole]): Option[Move] = None

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Move(orig, dest, promotion)

  override def toString = s"$piece ${toUci.uci}"
}
