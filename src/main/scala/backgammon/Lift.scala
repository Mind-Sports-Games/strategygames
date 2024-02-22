package strategygames.backgammon
import strategygames.{ MoveMetrics, Score }

import strategygames.backgammon.format.Uci

case class Lift(
    pos: Pos,
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def player = situationBefore.player

  def situationAfter =
    Situation(finalizeAfter, player)

  def finalizeAfter: Board = after updateHistory { h =>
    h.copy(
      currentTurn = h.currentTurn :+ toUci,
      score = Score(
        h.score.p1 + player.fold(1, 0),
        h.score.p2 + player.fold(0, 1)
      )
    )
  }

  def diceUsed = situationBefore.board.unusedDice.diff(after.unusedDice).head

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Lift(pos)

  override def toString = toUci.uci

}
