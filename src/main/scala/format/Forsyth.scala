package strategygames.format

import cats.implicits._
import strategygames._
import strategygames.variant.Variant

/** Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  */
object Forsyth {

  def initial(lib: GameLib): FEN = lib match {
    case GameLib.Draughts() => FEN.Draughts(draughts.format.Forsyth.initial)
    case GameLib.Chess()    => FEN.Chess(chess.format.Forsyth.initial)
  }

  def <<@(lib: GameLib, variant: Variant, fen: FEN): Option[Situation] = (lib, variant, fen) match {
    case (GameLib.Draughts(), Variant.Draughts(variant), FEN.Draughts(fen)) => draughts.format.Forsyth.<<@(variant, fen).map(Situation.Draughts)
    case (GameLib.Chess(), Variant.Chess(variant), FEN.Chess(fen))          => chess.format.Forsyth.<<@(variant, fen).map(Situation.Chess)
    case _ => sys.error("Mismatched gamelib types")
  }

  def <<(lib: GameLib, fen: FEN): Option[Situation] = (lib, fen) match {
    case (GameLib.Draughts(), FEN.Draughts(fen)) => draughts.format.Forsyth.<<(fen).map(Situation.Draughts)
    case (GameLib.Chess(), FEN.Chess(fen))       => chess.format.Forsyth.<<(fen).map(Situation.Chess)
    case _ => sys.error("Mismatched gamelib types")
  }

  case class SituationPlus(situation: Situation, fullMoveNumber: Int) {

    def turns = fullMoveNumber * 2 - situation.color.fold(2, 1)

  }

  def <<<@(lib: GameLib, variant: Variant, fen: FEN): Option[SituationPlus] = (lib, variant, fen) match {
    case (GameLib.Draughts(), Variant.Draughts(variant), FEN.Draughts(fen)) => draughts.format.Forsyth.<<<@(variant, fen).map(_ => SituationPlus(_.situation, _.fullMoveNumber))
    case (GameLib.Chess(), Variant.Chess(variant), FEN.Chess(fen))          => chess.format.Forsyth.<<<@(variant, fen).map(_ => SituationPlus(_.situation, _.fullMoveNumber))
    case _ => sys.error("Mismatched gamelib types")
  }

  def <<<(lib: GameLib, fen: FEN): Option[SituationPlus] = (lib, fen) match {
    case (GameLib.Draughts(), FEN.Draughts(fen)) => draughts.format.Forsyth.<<<@(fen).map(_ => SituationPlus(_.situation, _.fullMoveNumber))
    case (GameLib.Chess(), FEN.Chess(fen))       => chess.format.Forsyth.<<<@(fen).map(_ => SituationPlus(_.situation, _.fullMoveNumber))
    case _ => sys.error("Mismatched gamelib types")
  }

  // only cares about pieces positions on the board (first part of FEN string)
  def makeBoard(lib: GameLib, variant: Variant, fen: FEN): Option[Board] = (lib, variant, fen) match {
    case (GameLib.Draughts(), Variant.Draughts(variant), FEN.Draughts(fen)) => draughts.format.Forsyth.makeBoard(variant, fen).map(Board.Draughts)
    case (GameLib.Chess(), Variant.Chess(variant), FEN.Chess(fen))          => chess.format.Forsyth.makeBoard(variant, fen).map(Board.Chess)
    case _ => sys.error("Mismatched gamelib types")
  }

  def >>(situation: Situation): FEN = >>(SituationPlus(situation, 1))

  def >>(parsed: SituationPlus): FEN =
    parsed match {
      case SituationPlus(situation, _) => >>(Game(situation, turns = parsed.turns))
    }

  def >>(lib: GameLib, game: Game): FEN = (lib, game) match {
    case (GameLib.Draughts(), Game.Draughts(game)) => FEN.Draughts(draughts.format.Forsyth.>>(game))
    case (GameLib.Chess(), Game.Chess(game))       => FEN.Chess(chess.format.Forsyth.>>(game))
    case _ => sys.error("Mismatched gamelib types")
  }

}
