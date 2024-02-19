package strategygames.backgammon
import strategygames.MoveMetrics

import strategygames.backgammon.format.Uci

case class DiceRoll(
    dice: List[Int], // each entry gives a value for a dice
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def player = situationBefore.player

  def situationAfter = Situation(finalizeAfter, player)

  def withHistory(h: History) = copy(after = after withHistory h)

  def finalizeAfter: Board = after updateHistory { h =>
    h.copy(
      currentTurn = h.currentTurn :+ toUci
    )
  }

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.DiceRoll(dice)

  override def toString = toUci.uci
}
