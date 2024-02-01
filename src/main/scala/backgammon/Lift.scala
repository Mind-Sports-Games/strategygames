package strategygames.backgammon
import strategygames.MoveMetrics

import strategygames.backgammon.format.Uci

case class Lift(
    pos: Pos,
    situationBefore: Situation,
    after: Board,
    autoEndTurn: Boolean,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def player = situationBefore.player

  def situationAfter =
    Situation(finalizeAfter, if (autoEndTurn) player else player)

  def finalizeAfter: Board = after updateHistory { h =>
    h.copy(
      lastTurn = if (autoEndTurn) h.currentTurn :+ toUci else h.lastTurn,
      currentTurn = if (autoEndTurn) List() else h.currentTurn :+ toUci,
      halfMoveClock = h.halfMoveClock + (if (autoEndTurn && player == P2) 1 else 0)
    )
  }

  private def beforeBoard = situationBefore.board

  def diceUsed = (beforeBoard.firstPosIndex(!player) - pos.index).abs

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Drop(Role.defaultRole, pos)
  //def toUci = Uci.Lift(pos)

  override def toString = toUci.uci

}
