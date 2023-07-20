package strategygames
package format.pgn

import strategygames.chess

object Dumper {

  def apply(lib: GameLogic, data: Move): String = (lib, data) match {
    case (GameLogic.Draughts(), Move.Draughts(data))         => draughts.format.pdn.Dumper(data)
    case (GameLogic.Chess(), Move.Chess(data))               => chess.format.pgn.Dumper(data)
    case (GameLogic.FairySF(), Move.FairySF(data))           => fairysf.format.pgn.Dumper(data)
    case (GameLogic.Samurai(), Move.Samurai(data))           => samurai.format.pgn.Dumper(data)
    case (GameLogic.Togyzkumalak(), Move.Togyzkumalak(data)) => togyzkumalak.format.pgn.Dumper(data)
    case (GameLogic.Go(), _)                                 => sys.error("Mismatched gamelogic types 31 go has no moves")
    case _                                                   => sys.error("Mismatched gamelogic types 31")
  }

  def apply(lib: GameLogic, data: Drop): String = (lib, data) match {
    case (GameLogic.Chess(), Drop.Chess(data))     => chess.format.pgn.Dumper(data)
    case (GameLogic.FairySF(), Drop.FairySF(data)) => fairysf.format.pgn.Dumper(data)
    case (GameLogic.Go(), Drop.Go(data))           => go.format.pgn.Dumper(data)
    case _                                         => sys.error("Drops can only be applied to chess/fairysf/go")
  }

  def apply(lib: GameLogic, data: Pass): String = (lib, data) match {
    case (GameLogic.Go(), Pass.Go(data)) => go.format.pgn.Dumper(data)
    case _                               => sys.error("Pass can only be applied to go")
  }

  def apply(lib: GameLogic, data: Action): String = data match {
    case m: Move => apply(lib, m)
    case d: Drop => apply(lib, d)
    case p: Pass => apply(lib, p)
    case _       => sys.error("unknown action to apply to a game")
  }

}
