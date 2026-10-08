package strategygames.fiveinarow

import strategygames.{ MoveMetrics, Player }
import strategygames.fiveinarow.format.Uci

abstract class Action(
    situationBefore: Situation,
    after: Board,
    val metrics: MoveMetrics = MoveMetrics()
) {

  def before = situationBefore.board

  def player: Player = situationBefore.player

  def toUci: Uci

  def autoEndTurn: Boolean = after.playerToMove != player

  def finalizeAfter: Board = after.updateHistory(_.record(toUci, endsTurn = autoEndTurn))

  def situationAfter: Situation = Situation(finalizeAfter, after.playerToMove)

  lazy val lazySituationAfter: Situation = situationAfter

  def withMetrics(m: MoveMetrics): Action

  override def toString = toUci.uci

}
