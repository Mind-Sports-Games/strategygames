package strategygames
package format.pgn

import strategygames.chess

object Dumper {

  def apply(lib: GameLogic, data: Move): String = (lib, data) match {
    case (GameLogic.Draughts(), Move.Draughts(data)) => draughts.format.pdn.Dumper(data)
    case (GameLogic.Chess(), Move.Chess(data))       => chess.format.pgn.Dumper(data)
    case (GameLogic.FairySF(), Move.FairySF(data))   => fairysf.format.pgn.Dumper(data)
    case (GameLogic.Mancala(), Move.Mancala(data))   => mancala.format.pgn.Dumper(data)
    case _                                           => sys.error("Mismatched gamelogic types 31")
  }

  def apply(lib: GameLogic, data: Drop): String = (lib, data) match {
    case (GameLogic.Chess(), Drop.Chess(data))     => chess.format.pgn.Dumper(data)
    case (GameLogic.FairySF(), Drop.FairySF(data)) => fairysf.format.pgn.Dumper(data)
    case _                                         => sys.error("Drops can only be applied to chess/fairysf")
  }

}
