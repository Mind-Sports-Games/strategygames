package strategygames.format

import cats.data.Validated

import strategygames.variant.Variant
import strategygames.{ GameLogic, MoveOrDrop, Drop, Move }

object UciDump {

  def apply(
    lib: GameLogic,
    moves: Seq[String],
    initialFen: Option[FEN],
    variant: Variant,
    finalSquare: Boolean = false
  ): Validated[String, List[String]] = (lib, variant) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant))
      => strategygames.draughts.format.UciDump(moves, initialFen.map(_.toDraughts), variant, finalSquare)
    case (GameLogic.Chess(), Variant.Chess(variant))
      => strategygames.chess.format.UciDump(moves, initialFen.map(_.toChess), variant)
    case (GameLogic.FairySF(), Variant.FairySF(variant))
      => strategygames.fairysf.format.UciDump(moves, initialFen.map(_.toFairySF), variant)
    case (GameLogic.Mancala(), Variant.Mancala(variant))
      => strategygames.mancala.format.UciDump(moves, initialFen.map(_.toMancala), variant)
    case _ => sys.error("Mismatched gamelogic types 12")
  }

  def move(lib: GameLogic, variant: Variant)(mod: MoveOrDrop): String = (lib, variant, mod) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant), Left(Move.Draughts(mod)))
      => strategygames.draughts.format.UciDump.move(variant)(mod)
    case (GameLogic.Chess(), Variant.Chess(variant), Left(Move.Chess(mod)))
      => strategygames.chess.format.UciDump.move(variant)(Left(mod))
    case (GameLogic.Chess(), Variant.Chess(variant), Right(Drop.Chess(mod)))
      => strategygames.chess.format.UciDump.move(variant)(Right(mod)) 
    case (GameLogic.FairySF(), Variant.FairySF(variant), Left(Move.FairySF(mod)))
      => strategygames.fairysf.format.UciDump.move(variant)(Left(mod))
    case (GameLogic.FairySF(), Variant.FairySF(variant), Right(Drop.FairySF(mod)))
      => strategygames.fairysf.format.UciDump.move(variant)(Right(mod))
    case (GameLogic.Mancala(), Variant.Mancala(variant), Left(Move.Mancala(mod)))
      => strategygames.mancala.format.UciDump.move(variant)(mod)
    case _ => sys.error("Mismatched gamelogic types 13")
  }
}
