package strategygames.chess
import strategygames.chess.format.Uci
import strategygames.MoveMetrics
import strategygames.Player

abstract class Action(situationBefore: Situation) {
  def before = situationBefore.board
  def situationAfter: Situation
  def finalizeAfter: Board

  def withMetrics(m: MoveMetrics): Action

  def player: Player

  def toUci: Uci
}
