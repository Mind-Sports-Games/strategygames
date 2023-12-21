package strategygames.backgammon
import strategygames.MoveMetrics

import strategygames.backgammon.format.Uci

case class DiceRoll(
    dice: List[Int], // each entry gives a value for a dice
    situationBefore: Situation,
    after: Board,
    autoEndTurn: Boolean,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def playerAfter = if (autoEndTurn) !situationBefore.player else situationBefore.player

  def situationAfter =
    Situation(finalizeAfter, playerAfter)

  def withHistory(h: History) = copy(after = after withHistory h)

  // def finalizeAfter: Board = after updateHistory { h =>
  //  h.copy(
  //    lastTurn = if (autoEndTurn) h.currentTurn :+ toUci else h.lastTurn,
  //    currentTurn = if (autoEndTurn) List() else h.currentTurn :+ toUci
  //  )
  // }

  def player = situationBefore.player

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  // lazy val rollDice

  def toUci = Uci.DiceRoll(dice)

  override def toString = toUci.uci
}
