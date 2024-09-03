package strategygames
package format.pgn

import strategygames.{
  Action => StratAction,
  Drop => StratDrop,
  Move => StratMove,
  Pass => StratPass,
  SelectSquares => StratSelectSquares
}

object Dumper {

  def apply(lib: GameLogic, data: StratMove): String = (lib, data) match {
    case (GameLogic.Draughts(), StratMove.Draughts(data))         =>
      draughts.format.pdn.Dumper(data)
    case (GameLogic.Chess(), StratMove.Chess(data))               =>
      chess.format.pgn.Dumper(data)
    case (GameLogic.FairySF(), StratMove.FairySF(data))           =>
      fairysf.format.pgn.Dumper(data)
    case (GameLogic.Samurai(), StratMove.Samurai(data))           =>
      samurai.format.pgn.Dumper(data)
    case (GameLogic.Togyzkumalak(), StratMove.Togyzkumalak(data)) =>
      togyzkumalak.format.pgn.Dumper(data)
    case (GameLogic.Go(), _)                                      =>
      sys.error("Gamelogic Go has no moves, only drops")
    case (GameLogic.Backgammon(), StratMove.Backgammon(data))     =>
      backgammon.format.pgn.Dumper(data)
    case (GameLogic.Abalone(), StratMove.Abalone(data))           =>
      abalone.format.pgn.Dumper(data)
    case _                                                        =>
      sys.error("Mismatched gamelogic types 31")
  }

  def apply(lib: GameLogic, data: StratDrop): String = (lib, data) match {
    case (GameLogic.Chess(), StratDrop.Chess(data))     => chess.format.pgn.Dumper(data)
    case (GameLogic.FairySF(), StratDrop.FairySF(data)) => fairysf.format.pgn.Dumper(data)
    case (GameLogic.Go(), StratDrop.Go(data))           => go.format.pgn.Dumper(data)
    case _                                              => sys.error("Drops can only be applied to chess/fairysf/go")
  }

  def apply(lib: GameLogic, data: StratPass): String = (lib, data) match {
    case (GameLogic.Go(), StratPass.Go(data)) => go.format.pgn.Dumper(data)
    case _                                    => sys.error("Pass can only be applied to go")
  }

  def apply(lib: GameLogic, data: StratSelectSquares): String = (lib, data) match {
    case (GameLogic.Go(), StratSelectSquares.Go(data)) => go.format.pgn.Dumper(data)
    case _                                             => sys.error("SelectSquares can only be applied to go")
  }

  def apply(lib: GameLogic, data: StratAction): String = data match {
    case m: StratMove           => apply(lib, m)
    case d: StratDrop           => apply(lib, d)
    case p: StratPass           => apply(lib, p)
    case ss: StratSelectSquares => apply(lib, ss)
    case _                      => sys.error("unknown action to apply to a game")
  }

}
