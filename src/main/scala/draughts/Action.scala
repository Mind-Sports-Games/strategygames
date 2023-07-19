package strategygames.draughts
import strategygames.draughts.format.Uci
import strategygames.MoveMetrics
import strategygames.Player

abstract class Action(situationBefore: Situation, after: Board, metrics: MoveMetrics = MoveMetrics()) {
  def before = situationBefore.board

  def situationAfter: Situation
  def situationAfter(finalSquare: Boolean): Situation

  def player: Player

  def finalizeAfter(finalSquare: Boolean = false): Board
  def withMetrics(m: MoveMetrics): Action

  def toUci: Uci
}
