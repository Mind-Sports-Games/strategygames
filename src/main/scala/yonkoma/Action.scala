package strategygames.yonkoma
import strategygames.yonkoma.format.Uci
import strategygames.MoveMetrics
import strategygames.Player

import scala.annotation.nowarn

abstract class Action(
    situationBefore: Situation,
    @nowarn after: Board,
    @nowarn metrics: MoveMetrics = MoveMetrics()
) {

  def situationAfter: Situation
  def finalizeAfter: Board

  def player: Player = situationBefore.player

  def withMetrics(m: MoveMetrics): Action
  def toUci: Uci

}
