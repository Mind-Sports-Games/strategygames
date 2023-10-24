package strategygames.format

import cats.data.Validated

import strategygames.variant.Variant
import strategygames.{ Action, Actions, Drop, GameFamily, GameLogic, Move, Pass, SelectSquares }

object UciDump {

  def apply(
      lib: GameLogic,
      actions: Actions,
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, Actions] = (lib, variant) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant))         =>
      strategygames.draughts.format.UciDump(actions, initialFen.map(_.toDraughts), variant, finalSquare)
    case (GameLogic.Chess(), Variant.Chess(variant))               =>
      strategygames.chess.format.UciDump(actions, initialFen.map(_.toChess), variant)
    case (GameLogic.FairySF(), Variant.FairySF(variant))           =>
      strategygames.fairysf.format.UciDump(actions, initialFen.map(_.toFairySF), variant)
    case (GameLogic.Samurai(), Variant.Samurai(variant))           =>
      strategygames.samurai.format.UciDump(actions, initialFen.map(_.toSamurai), variant)
    case (GameLogic.Togyzkumalak(), Variant.Togyzkumalak(variant)) =>
      strategygames.togyzkumalak.format.UciDump(actions, initialFen.map(_.toTogyzkumalak), variant)
    case (GameLogic.Go(), Variant.Go(variant))                     =>
      strategygames.go.format.UciDump(actions, initialFen.map(_.toGo), variant)
    case _                                                         => sys.error("Mismatched gamelogic types 12")
  }

  def action(lib: GameLogic, variant: Variant)(a: Action): String = (lib, variant, a) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant), Move.Draughts(a))             =>
      strategygames.draughts.format.UciDump.action(variant)(a)
    case (GameLogic.Chess(), Variant.Chess(variant), Move.Chess(a))                      =>
      strategygames.chess.format.UciDump.action(variant)(a)
    case (GameLogic.Chess(), Variant.Chess(variant), Drop.Chess(a))                      =>
      strategygames.chess.format.UciDump.action(variant)(a)
    case (GameLogic.FairySF(), Variant.FairySF(variant), Move.FairySF(a))                =>
      strategygames.fairysf.format.UciDump.action(variant)(a)
    case (GameLogic.FairySF(), Variant.FairySF(variant), Drop.FairySF(a))                =>
      strategygames.fairysf.format.UciDump.action(variant)(a)
    case (GameLogic.Samurai(), Variant.Samurai(variant), Move.Samurai(a))                =>
      strategygames.samurai.format.UciDump.action(variant)(a)
    case (GameLogic.Togyzkumalak(), Variant.Togyzkumalak(variant), Move.Togyzkumalak(a)) =>
      strategygames.togyzkumalak.format.UciDump.action(variant)(a)
    case (GameLogic.Go(), Variant.Go(variant), Drop.Go(a))                               =>
      strategygames.go.format.UciDump.action(variant)(a)
    case (GameLogic.Go(), Variant.Go(variant), Pass.Go(a))                               =>
      strategygames.go.format.UciDump.action(variant)(a)
    case (GameLogic.Go(), Variant.Go(variant), SelectSquares.Go(a))                      =>
      strategygames.go.format.UciDump.action(variant)(a)
    case _                                                                               => sys.error("Mismatched gamelogic types 13")
  }

  private def moveStringToUci(variant: Variant, moves: String): List[Uci] =
    moves.trim().split(" ") match {
      case Array("") => Nil
      case l         => l.toList.flatMap(m => Uci.apply(variant.gameLogic, variant.gameFamily, m))
    }

  private def moveStringToLexicalUci(moves: String): List[LexicalUci] =
    moves.trim().split(" ") match {
      case Array("") => Nil
      case l         => l.toList.flatMap(m => LexicalUci.apply(m))
    }

  def fishnetUci(variant: Variant, moves: String): String =
    fishnetUci(variant, moveStringToUci(variant, moves))

  def fishnetUci(variant: Variant, moves: List[Uci]): String = variant match {
    case Variant.FairySF(variant) =>
      strategygames.fairysf.format.UciDump.toFishnetUci(variant.gameFamily, moves.map(_.toFairySF))
    case _                        =>
      moves.map(_.fishnetUci).mkString(" ")
  }

  def fromFishnetUci(variant: Variant, moves: List[LexicalUci]): List[LexicalUci] = variant match {
    case Variant.FairySF(variant) =>
      strategygames.fairysf.format.UciDump.fromFishnetUci(variant, moves)
    case _                        =>
      moves
  }

  def fromFishnetUci(variant: Variant, moves: String): List[LexicalUci] =
    fromFishnetUci(variant, moveStringToLexicalUci(moves))
}
