package strategygames.abalone

import strategygames.abalone.format.Uci
import strategygames.{MoveMetrics, Player}

case class Move(
    player: Player,
    orig: Pos,
    dest: Pos,
    situationBefore: Situation,
    after: Board,
    autoEndTurn: Boolean,
    capture: Option[Pos] = None,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {
  def withHistory(h: History) = copy(after = after withHistory h)

  override def finalizeAfter: Board = after.variant.finalizeBoardAfter(this)

  override def situationAfter = Situation(finalizeAfter, if (autoEndTurn) !player else player)

  def applyVariantEffect: Move = before.variant addVariantEffect this

  // does this move capture an opponent piece?
  def captures = capture.isDefined

  override def withMetrics(m: MoveMetrics) = copy(metrics = m)

  override def toUci = Uci.Move(orig, dest)

  override def toString = s"${player.number} ${toUci.uci}"
}
