package strategygames.abalone

import strategygames.MoveMetrics
import strategygames.abalone.format.Uci

import scala.annotation.nowarn

abstract class Action(
                       situationBefore: Situation,
                       after: Board,
                       @nowarn metrics: MoveMetrics = MoveMetrics()
                     ) {
  def before: Board = situationBefore.board

  def situationAfter: Situation//TODO Alex?

  def finalizeAfter: Board = after//TODO Alex?

  def withMetrics(m: MoveMetrics): Action

  def toUci: Uci
}