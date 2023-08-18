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
    case d: go.Drop           => Drop.Go(d)
    case p: go.Pass           => Pass.Go(p)
    case ss: go.SelectSquares => SelectSquares.Go(ss)
  }

  def toChess(action: Action): chess.Action = action match {
    case Move.Chess(m) => m
    case Drop.Chess(d) => d
    case _             => sys.error("Expecting a chess action e.g. move or drop")
  }

  def toDraughts(action: Action): draughts.Move = action match {
    case Move.Draughts(m) => m
    case _                => sys.error("Expecting a draughts action e.g. move")
  }

  def toFairySF(action: Action): fairysf.Action = action match {
    case Move.FairySF(m) => m
    case Drop.FairySF(d) => d
    case _               => sys.error("Expecting a fairysf action e.g. move or drop")
  }

  def toSamurai(action: Action): samurai.Move = action match {
    case Move.Samurai(m) => m
    case _               => sys.error("Expecting a samurai action e.g. move")
  }

  def toTogyzkumalak(action: Action): togyzkumalak.Move = action match {
    case Move.Togyzkumalak(m) => m
    case _                    => sys.error("Expecting a togyzkumalak action e.g. move")
  }

  def toGo(action: Action): go.Action = action match {
    case Drop.Go(d)           => d
    case Pass.Go(p)           => p
    case SelectSquares.Go(ss) => ss
    case _                    => sys.error("Expecting a go action e.g. drop or pass or SelectSquares")
  }
}
