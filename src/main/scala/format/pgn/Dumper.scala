package strategygames
package format.pgn

import strategygames.{ Drop => StratDrop, Move => StratMove }

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
    case _                                                        =>
      sys.error("Mismatched gamelogic types 31")
  }

  def apply(lib: GameLogic, data: StratDrop): String = (lib, data) match {
    case (GameLogic.Chess(), StratDrop.Chess(data))     => chess.format.pgn.Dumper(data)
    case (GameLogic.FairySF(), StratDrop.FairySF(data)) => fairysf.format.pgn.Dumper(data)
    case _                                              => sys.error("Drops can only be applied to chess/fairysf")
  }

}
