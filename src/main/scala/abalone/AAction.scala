package abalone

import strategygames.abalone.format.Uci
import strategygames.{MoveMetrics, Player}

import scala.annotation.nowarn

abstract class AAction(
                        situationBefore: SSituation,
                        after: BBoard,
                        @nowarn metrics: MoveMetrics = MoveMetrics()
                      ) {
  def before = situationBefore.board

  def situationAfter: SSituation

  def finalizeAfter: BBoard = after

  def player: Player

  def withMetrics(m: MoveMetrics): AAction

  def toUci: Uci
}