package strategygames
package format.pgn

import strategygames.chess

object Dumper {

  def apply(lib: GameLogic, situation: Situation, data: Move, next: Situation): String =
    (lib, situation, data, next) match {
      case (
        GameLogic.Draughts(),
        Situation.Draughts(situation),
        Move.Draughts(data),
        Situation.Draughts(next)
      ) => draughts.format.pdn.Dumper.apply(situation, data, next)
      case (
        GameLogic.Chess(),
        Situation.Chess(situation),
        Move.Chess(data),
        Situation.Chess(next)
      ) => chess.format.pgn.Dumper.apply(situation, data, next)
      case _ => sys.error("Mismatched gamelogic types 30")
  }

  def apply(lib: GameLogic, data: Move): String = (lib, data) match {
    case (GameLogic.Draughts(), Move.Draughts(data)) => draughts.format.pdn.Dumper.apply(data)
    case (GameLogic.Chess(), Move.Chess(data))       => chess.format.pgn.Dumper.apply(data)
    case _ => sys.error("Mismatched gamelogic types 31")
  }

  def apply(lib: GameLogic, data: chess.Drop): String = lib match {
    case GameLogic.Chess()      => chess.format.pgn.Dumper.apply(data)
    case _ => sys.error("Drops can only be applied ")
  }

}
