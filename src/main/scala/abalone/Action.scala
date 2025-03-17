package strategygames.abalone

import strategygames.abalone.format.Uci
import strategygames.{MoveMetrics, Player}

import scala.annotation.nowarn

abstract class Action(
                       situationBefore: Situation,
                       after: Board,
                       @nowarn metrics: MoveMetrics = MoveMetrics()
                     ) {
  def before: Board = situationBefore.board

  def situationAfter: Situation

  def finalizeAfter: Board = after

  def withMetrics(m: MoveMetrics): Action

  def toUci: Uci
}