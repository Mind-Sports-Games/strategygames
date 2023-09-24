package strategygames.format

import cats.data.Validated

import strategygames.variant.Variant
import strategygames.{ Action, Drop, GameLogic, Move, Pass, SelectSquares }

object UciDump {

  def apply(
      lib: GameLogic,
      moves: Seq[String],
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, List[String]] = (lib, variant) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant))         =>
      strategygames.draughts.format.UciDump(moves, initialFen.map(_.toDraughts), variant, finalSquare)
    case (GameLogic.Chess(), Variant.Chess(variant))               =>
      strategygames.chess.format.UciDump(moves, initialFen.map(_.toChess), variant)
    case (GameLogic.FairySF(), Variant.FairySF(variant))           =>
      strategygames.fairysf.format.UciDump(moves, initialFen.map(_.toFairySF), variant)
    case (GameLogic.Samurai(), Variant.Samurai(variant))           =>
      strategygames.samurai.format.UciDump(moves, initialFen.map(_.toSamurai), variant)
    case (GameLogic.Togyzkumalak(), Variant.Togyzkumalak(variant)) =>
      strategygames.togyzkumalak.format.UciDump(moves, initialFen.map(_.toTogyzkumalak), variant)
    case (GameLogic.Go(), Variant.Go(variant))                     =>
      strategygames.go.format.UciDump(moves, initialFen.map(_.toGo), variant)
    case _                                                         => sys.error("Mismatched gamelogic types 12")
  }

  def move(lib: GameLogic, variant: Variant)(action: Action): String = (lib, variant, action) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant), Move.Draughts(action))             =>
      strategygames.draughts.format.UciDump.move(variant)(action)
    case (GameLogic.Chess(), Variant.Chess(variant), Move.Chess(action))                      =>
      strategygames.chess.format.UciDump.move(variant)(action)
    case (GameLogic.Chess(), Variant.Chess(variant), Drop.Chess(action))                      =>
      strategygames.chess.format.UciDump.move(variant)(action)
    case (GameLogic.FairySF(), Variant.FairySF(variant), Move.FairySF(action))                =>
      strategygames.fairysf.format.UciDump.move(variant)(action)
    case (GameLogic.FairySF(), Variant.FairySF(variant), Drop.FairySF(action))                =>
      strategygames.fairysf.format.UciDump.move(variant)(action)
    case (GameLogic.Samurai(), Variant.Samurai(variant), Move.Samurai(action))                =>
      strategygames.samurai.format.UciDump.move(variant)(action)
    case (GameLogic.Togyzkumalak(), Variant.Togyzkumalak(variant), Move.Togyzkumalak(action)) =>
      strategygames.togyzkumalak.format.UciDump.move(variant)(action)
    case (GameLogic.Go(), Variant.Go(_), Drop.Go(action))                                     =>
      strategygames.go.format.UciDump.move(action)
    case (GameLogic.Go(), Variant.Go(_), Pass.Go(action))                                     =>
      strategygames.go.format.UciDump.move(action)
    case (GameLogic.Go(), Variant.Go(_), SelectSquares.Go(action))                            =>
      strategygames.go.format.UciDump.move(action)
    case _                                                                                    => sys.error("Mismatched gamelogic types 13")
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
