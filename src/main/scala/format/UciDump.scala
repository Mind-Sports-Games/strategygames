package strategygames.format

import cats.data.Validated

import strategygames.variant.Variant
import strategygames.{ GameLogic, MoveOrDrop, Move, Replay }

object UciDump {

  // a2a4, b8c6
  def apply(lib: GameLogic, replay: Replay): List[String] = (lib, replay) match {
    case (GameLogic.Draughts(), Replay.Draughts(replay))
      => strategygames.draughts.format.UciDump.apply(replay)
    case (GameLogic.Chess(), Replay.Chess(replay))
      => strategygames.chess.format.UciDump.apply(replay)
    case _ => sys.error("Mismatched gamelogic types 11")
  }

  def apply(
    lib: GameLogic,
    moves: Seq[String],
    initialFen: Option[FEN],
    variant: Variant,
    finalSquare: Boolean = false
  ): Validated[String, List[String]] = (lib, variant) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant))
      => strategygames.draughts.format.UciDump.apply(moves, initialFen.map(_.toDraughts), variant, finalSquare)
    case (GameLogic.Chess(), Variant.Chess(variant))
      => strategygames.chess.format.UciDump.apply(moves, initialFen.map(_.toChess), variant)
    case _ => sys.error("Mismatched gamelogic types 12")
  }

  def move(lib: GameLogic, variant: Variant)(mod: MoveOrDrop): String = (lib, variant, mod) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant), Left(Move.Draughts(mod)))
      => strategygames.draughts.format.UciDump.move(variant)(mod)
    case (GameLogic.Chess(), Variant.Chess(variant), Left(Move.Chess(mod)))
      => strategygames.chess.format.UciDump.move(variant)(Left(mod))
    case (GameLogic.Chess(), Variant.Chess(variant), mod: strategygames.chess.Drop)
      => strategygames.chess.format.UciDump.move(variant)(Right(mod)) 
    case _ => sys.error("Mismatched gamelogic types 13")
  }
}
