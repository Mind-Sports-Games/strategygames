package strategygames.format

import cats.data.Validated

import strategygames.variant.Variant
import strategygames.{ GameLib, MoveOrDrop, Move, Replay }

object UciDump {

  // a2a4, b8c6
  def apply(lib: GameLib, replay: Replay): List[String] = (lib, replay) match {
    case (GameLib.Draughts(), Replay.Draughts(replay))
      => strategygames.draughts.format.UciDump.apply(replay)
    case (GameLib.Chess(), Replay.Chess(replay))
      => strategygames.chess.format.UciDump.apply(replay)
    case _ => sys.error("Mismatched gamelib types 11")
  }

  def apply(
    lib: GameLib,
    moves: Seq[String],
    initialFen: Option[FEN],
    variant: Variant,
    finalSquare: Boolean = false
  ): Validated[String, List[String]] = (lib, variant) match {
    case (GameLib.Draughts(), Variant.Draughts(variant))
      => strategygames.draughts.format.UciDump.apply(moves, initialFen.map(_.toDraughts), variant, finalSquare)
    case (GameLib.Chess(), Variant.Chess(variant))
      => strategygames.chess.format.UciDump.apply(moves, initialFen.map(_.toChess), variant)
    case _ => sys.error("Mismatched gamelib types 12")
  }

  def move(lib: GameLib, variant: Variant)(mod: MoveOrDrop): String = (lib, variant, mod) match {
    case (GameLib.Draughts(), Variant.Draughts(variant), Left(Move.Draughts(mod)))
      => strategygames.draughts.format.UciDump.move(variant)(mod)
    case (GameLib.Chess(), Variant.Chess(variant), Left(Move.Chess(mod)))
      => strategygames.chess.format.UciDump.move(variant)(Left(mod))
    case (GameLib.Chess(), Variant.Chess(variant), mod: strategygames.chess.Drop)
      => strategygames.chess.format.UciDump.move(variant)(Right(mod)) 
    case _ => sys.error("Mismatched gamelib types 13")
  }
}
