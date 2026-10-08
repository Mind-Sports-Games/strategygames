package strategygames
package format.pgn

import strategygames.{
  Action => StratAction,
  DrawCounter => StratDrawCounter,
  Drop => StratDrop,
  Move => StratMove,
  Pass => StratPass,
  SelectSquares => StratSelectSquares,
  Swap => StratSwap,
  Swap2 => StratSwap2
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
    case (GameLogic.Dameo(), StratMove.Dameo(data))               =>
      dameo.format.pdn.Dumper(data)
    case (GameLogic.Entropy(), StratMove.Entropy(data))           =>
      entropy.format.pgn.Dumper(data)
    case _                                                        =>
      sys.error("Mismatched gamelogic types 31")
  }

  def apply(lib: GameLogic, data: StratDrop): String = (lib, data) match {
    case (GameLogic.Chess(), StratDrop.Chess(data))           => chess.format.pgn.Dumper(data)
    case (GameLogic.FairySF(), StratDrop.FairySF(data))       => fairysf.format.pgn.Dumper(data)
    case (GameLogic.Go(), StratDrop.Go(data))                 => go.format.pgn.Dumper(data)
    case (GameLogic.Entropy(), StratDrop.Entropy(data))       => entropy.format.pgn.Dumper(data)
    case (GameLogic.FiveInARow(), StratDrop.FiveInARow(data)) => fiveinarow.format.pgn.Dumper(data)
    case _                                                    => sys.error("Drops can only be applied to chess/fairysf/go")
  }

  def apply(lib: GameLogic, data: StratPass): String = (lib, data) match {
    case (GameLogic.Go(), StratPass.Go(data))           => go.format.pgn.Dumper(data)
    case (GameLogic.Entropy(), StratPass.Entropy(data)) => entropy.format.pgn.Dumper(data)
    case _                                              => sys.error("Pass can only be applied to go/entropy")
  }

  def apply(lib: GameLogic, data: StratSelectSquares): String = (lib, data) match {
    case (GameLogic.Go(), StratSelectSquares.Go(data)) => go.format.pgn.Dumper(data)
    case _                                             => sys.error("SelectSquares can only be applied to go")
  }

  def apply(lib: GameLogic, data: StratDrawCounter): String = (lib, data) match {
    case (GameLogic.Entropy(), StratDrawCounter.Entropy(data)) => entropy.format.pgn.Dumper(data)
    case _                                                     =>
      sys.error("DrawCounter can only be applied to entropy")
  }

  def apply(lib: GameLogic, data: StratSwap): String = (lib, data) match {
    case (GameLogic.FiveInARow(), StratSwap.FiveInARow(data)) => fiveinarow.format.pgn.Dumper(data)
    case _                                                    => sys.error("Swap can only be applied to fiveinarow")
  }

  def apply(lib: GameLogic, data: StratSwap2): String = (lib, data) match {
    case (GameLogic.FiveInARow(), StratSwap2.FiveInARow(data)) => fiveinarow.format.pgn.Dumper(data)
    case _                                                     => sys.error("Swap2 can only be applied to fiveinarow")
  }

  def apply(lib: GameLogic, data: StratAction): String = data match {
    case m: StratMove           => apply(lib, m)
    case d: StratDrop           => apply(lib, d)
    case p: StratPass           => apply(lib, p)
    case ss: StratSelectSquares => apply(lib, ss)
    case dc: StratDrawCounter   => apply(lib, dc)
    case s: StratSwap           => apply(lib, s)
    case s2: StratSwap2         => apply(lib, s2)
    case _                      => sys.error("unknown action to apply to a game")
  }

}
