package strategygames.go
import strategygames.go.format.Uci
import strategygames.MoveMetrics
import strategygames.Player

abstract class Action(
    situationBefore: Situation,
    after: Board
) {
  def before = situationBefore.board

  def situationAfter: Situation
  def finalizeAfter: Board

  def player: Player

  def applyVariantEffect: Action
  def withMetrics(m: MoveMetrics): Action

  def toUci: Uci

}
