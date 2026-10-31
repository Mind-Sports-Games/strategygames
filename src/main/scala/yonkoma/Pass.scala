package strategygames.yonkoma
import strategygames.MoveMetrics

import strategygames.yonkoma.format.Uci

// a skipped turn: the only action of a player who has no legal move
case class Pass(
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def player = situationBefore.player

  def situationAfter = Situation(finalizeAfter, !player)

  def finalizeAfter: Board = after updateHistory { h =>
    h.copy(
      lastTurn = h.currentTurn :+ toUci,
      currentTurn = List(),
      halfMoveClock = h.halfMoveClock + 1
    )
  }

  lazy val lazySituationAfter = situationAfter

  def withMetrics(m: MoveMetrics): Pass = copy(metrics = m)

  def toUci: Uci.Pass = Uci.Pass()

  override def toString = toUci.uci

}
