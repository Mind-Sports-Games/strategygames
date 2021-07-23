package strategygames
package format.pgn

object Dumper {

  def apply(lib: GameLib, situation: Situation, data: Move, next: Situation): String =
    (lib, situation, data, next) match {
      case (
        GameLib.Draughts(),
        Situation.Draughts(situation),
        Move.Draughts(data),
        Situation.Draughts(next)
      ) => draughts.format.pdn.Dumper.apply(situation, data, next)
      case (
        GameLib.Chess(),
        Situation.Chess(situation),
        Move.Chess(data),
        Situation.Chess(next)
      ) => chess.format.pgn.Dumper.apply(situation, data, next)
      case _ => sys.error("Mismatched gamelib types")
  }

  def apply(lib: GameLib, data: Move): String = (lib, data) match {
    case (GameLib.Draughts(), Move.Draughts(data)) => draughts.format.pdn.Dumper.apply(data)
    case (GameLib.Chess(), Move.Chess(data))       => chess.format.pgn.Dumper.apply(data)
    case _ => sys.error("Mismatched gamelib types")
  }

}
