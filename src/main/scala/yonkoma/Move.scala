package strategygames.yonkoma
import strategygames.MoveMetrics

import strategygames.yonkoma.format.Uci

case class Move(
    piece: Piece,
    orig: Pos,
    dest: Pos,
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def player = piece.player

  def situationAfter = Situation(finalizeAfter, !player)

  def finalizeAfter: Board = after updateHistory { h =>
    h.copy(
      lastTurn = h.currentTurn :+ toUci,
      currentTurn = List(),
      halfMoveClock = h.halfMoveClock + 1
    )
  }

  lazy val lazySituationAfter = situationAfter

  def withMetrics(m: MoveMetrics): Move = copy(metrics = m)

  def toUci: Uci.Move = Uci.Move(orig, dest)

  override def toString = s"$piece ${toUci.uci}"
}
