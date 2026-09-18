package strategygames.entropy
import strategygames.MoveMetrics

import strategygames.entropy.format.Uci

case class Drop(
    piece: Piece,
    pos: Pos,
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def player = situationBefore.player

  def situationAfter = Situation(finalizeAfter, !player)

  def finalizeAfter: Board = after.afterTurnBy(player) updateHistory { h =>
    h.copy(
      lastTurn = h.currentTurn :+ toUci,
      currentTurn = List(),
      halfMoveClock = h.halfMoveClock + 1
    )
  }

  lazy val lazySituationAfter = situationAfter

  def withMetrics(m: MoveMetrics): Drop = copy(metrics = m)

  def toUci: Uci.Drop = Uci.Drop(piece.role, pos)

  override def toString = toUci.uci

}
