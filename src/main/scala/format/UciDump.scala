package strategygames.format

import cats.data.Validated

import strategygames.variant.Variant
import strategygames.{ GameLib, MoveOrDrop, Replay }

object UciDump {

  // a2a4, b8c6
  def apply(replay: Replay): List[String] =
    replay.chronoMoves map move(replay.setup.board.variant)

  def move(lib: GameLib, variant: Variant)(mod: MoveOrDrop): String = (lib, variant, mod) match {
    case (GameLib.Draughts(), Variant.Draughts(variant), Move.Draughts(mod)) => draughts.format.UciDump.move(variant)(mod) 
    case (GameLib.Chess(), Variant.Chess(variant), Move.Chess(mod)) => chess.format.UciDump.move(variant)(mod) 
    case (GameLib.Chess(), Variant.Chess(variant), chess.Drop(mod)) => chess.format.UciDump.move(variant)(mod) 
    case _ => sys.error("Mismatched gamelib types")
  }
}
