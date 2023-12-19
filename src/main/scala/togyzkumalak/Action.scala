package strategygames.togyzkumalak
import strategygames.togyzkumalak.format.Uci
import strategygames.MoveMetrics
import strategygames.Player

abstract class Action(situationBefore: Situation) {
  def before = situationBefore.board

  def situationAfter: Situation
  def finalizeAfter: Board

  def player: Player

  def withMetrics(m: MoveMetrics): Action
  def toUci: Uci

}
