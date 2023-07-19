package strategygames

import cats.syntax.option.none

import strategygames.format.Uci

abstract class Action(
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) {
  def situationAfter: Situation
  // def finalizeAfter: Board //this can be added once draughts has been refactored (removing the input finalSquare)
  def before = situationBefore.board

  def player: Player
  def toUci: Uci

  def toChess: chess.Action
  def toDraughts: draughts.Action
  def toFairySF: fairysf.Action
  def toSamurai: samurai.Action
  def toTogyzkumalak: togyzkumalak.Action
  def toGo: go.Action
}

object Action {
  def wrap(action: chess.Action): Action = action match {
    case m: chess.Move => Move.Chess(m)
    case d: chess.Drop => Drop.Chess(d)
  }

  def wrap(action: draughts.Action): Action = action match {
    case m: draughts.Move => Move.Draughts(m)
  }

  def wrap(action: fairysf.Action): Action = action match {
    case m: fairysf.Move => Move.FairySF(m)
    case d: fairysf.Drop => Drop.FairySF(d)
  }

  def wrap(action: samurai.Action): Action = action match {
    case m: samurai.Move => Move.Samurai(m)
  }

  def wrap(action: togyzkumalak.Action): Action = action match {
    case m: togyzkumalak.Move => Move.Togyzkumalak(m)
  }

  def wrap(action: go.Action): Action = action match {
    case d: go.Drop => Drop.Go(d)
    case p: go.Pass => Pass.Go(p)
  }
}
