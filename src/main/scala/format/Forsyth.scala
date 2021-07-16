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
    case (GameLib.Draughts(), Variant.Draughts(variant), FEN.Draughts(fen))
      => draughts.format.Forsyth.<<<@(variant, fen).map(
        sp => SituationPlus(Situation.Draughts(sp.situation), sp.fullMoveNumber)
      )
    case (GameLib.Chess(), Variant.Chess(variant), FEN.Chess(fen))
      => chess.format.Forsyth.<<<@(variant, fen).map(
        sp => SituationPlus(Situation.Chess(sp.situation), sp.fullMoveNumber)
      )
    case _ => sys.error("Mismatched gamelib types")
  }

  def <<<(lib: GameLib, fen: FEN): Option[SituationPlus] = (lib, fen) match {
    case (GameLib.Draughts(), FEN.Draughts(fen)) => draughts.format.Forsyth.<<<(fen).map(
      sp => SituationPlus(Situation.Draughts(sp.situation), sp.fullMoveNumber)
    )
    case (GameLib.Chess(), FEN.Chess(fen))       => chess.format.Forsyth.<<<(fen).map(
      sp => SituationPlus(Situation.Chess(sp.situation), sp.fullMoveNumber)
    )
    case _ => sys.error("Mismatched gamelib types")
  }

  // only cares about pieces positions on the board (first part of FEN string)
  def makeBoard(lib: GameLib, variant: Variant, fen: FEN): Option[Board] = (lib, variant, fen) match {
    case (GameLib.Draughts(), Variant.Draughts(variant), FEN.Draughts(fen)) => draughts.format.Forsyth.makeBoard(variant, fen).map(Board.Draughts)
    case (GameLib.Chess(), Variant.Chess(variant), FEN.Chess(fen))          => chess.format.Forsyth.makeBoard(variant, fen).map(Board.Chess)
    case _ => sys.error("Mismatched gamelib types")
  }

  def >>(lib: GameLib, situation: Situation): FEN = >>(lib, SituationPlus(situation, 1))

  def >>(lib: GameLib, parsed: SituationPlus): FEN = (lib, parsed.situation) match{
    case (GameLib.Draughts(), Situation.Draughts(situation))
      => FEN.Draughts(draughts.format.Forsyth.>>(
        draughts.format.Forsyth.SituationPlus(situation, parsed.fullMoveNumber)
      ))
    case (GameLib.Chess(), Situation.Chess(situation))
      => FEN.Chess(chess.format.Forsyth.>>(
        chess.format.Forsyth.SituationPlus(situation, parsed.fullMoveNumber)
      ))
    case _ => sys.error("Mismatched gamelib types")
  }

  def >>(lib: GameLib, game: Game): FEN = (lib, game) match {
    case (GameLib.Draughts(), Game.Draughts(game)) => FEN.Draughts(draughts.format.Forsyth.>>(game))
    case (GameLib.Chess(), Game.Chess(game))       => FEN.Chess(chess.format.Forsyth.>>(game))
    case _ => sys.error("Mismatched gamelib types")
  }

  def exportBoard(lib: GameLib, board: Board, algebraic: Boolean = false): String = (lib, board) match {
    case (GameLib.Draughts(), Board.Draughts(board)) => draughts.format.Forsyth.exportBoard(board, algebraic)
    case (GameLib.Chess(), Board.Chess(board))       => chess.format.Forsyth.exportBoard(board)
    case _ => sys.error("Mismatched gamelib types")
  }

  def boardAndColor(lib: GameLib, situation: Situation): String =
    boardAndColor(lib, situation.board, situation.color)

  def boardAndColor(lib: GameLib, board: Board, turnColor: Color): String = (lib, board, turnColor) match {
    case (GameLib.Draughts(), Board.Draughts(board), Color.Draughts(turnColor))
      => draughts.format.Forsyth.boardAndColor(board, turnColor)
    case (GameLib.Chess(), Board.Chess(board), Color.Chess(turnColor))
      => chess.format.Forsyth.boardAndColor(board, turnColor)
    case _ => sys.error("Mismatched gamelib types")
  }

}
