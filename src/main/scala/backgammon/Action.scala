package strategygames.backgammon
import strategygames.backgammon.format.Uci
import strategygames.MoveMetrics
import strategygames.Player

import scala.annotation.nowarn

abstract class Action(
    situationBefore: Situation,
    @nowarn after: Board,
    @nowarn metrics: MoveMetrics = MoveMetrics()
) {

  def before = situationBefore.board

  def situationAfter: Situation
  def finalizeAfter: Board

  def player: Player

  def withMetrics(m: MoveMetrics): Action
  def toUci: Uci

}
