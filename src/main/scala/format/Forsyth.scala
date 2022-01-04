package strategygames.format

import cats.implicits._
import strategygames._
import strategygames.variant.Variant

/** Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  */
object Forsyth {

  def initial(lib: GameLogic): FEN = lib match {
    case GameLogic.Draughts() => FEN.Draughts(draughts.format.Forsyth.initial)
    case GameLogic.Chess()    => FEN.Chess(chess.format.Forsyth.initial)
    case GameLogic.FairySF()  => FEN.FairySF(fairysf.format.Forsyth.initial)
  }

  def <<@(lib: GameLogic, variant: Variant, fen: FEN): Option[Situation] =
    (lib, variant, fen) match {
      case (GameLogic.Draughts(), Variant.Draughts(variant), FEN.Draughts(fen))
        => draughts.format.Forsyth.<<@(variant, fen).map(Situation.Draughts)
      case (GameLogic.Chess(), Variant.Chess(variant), FEN.Chess(fen))
        => chess.format.Forsyth.<<@(variant, fen).map(Situation.Chess)
      case (GameLogic.FairySF(), Variant.FairySF(variant), FEN.FairySF(fen))
        => fairysf.format.Forsyth.<<@(variant, fen).map(Situation.FairySF)
      case _ => sys.error("Mismatched gamelogic types 14")
  }

  def <<(lib: GameLogic, fen: FEN): Option[Situation] = (lib, fen) match {
    case (GameLogic.Draughts(), FEN.Draughts(fen))
      => draughts.format.Forsyth.<<(fen).map(Situation.Draughts)
    case (GameLogic.Chess(), FEN.Chess(fen))
      => chess.format.Forsyth.<<(fen).map(Situation.Chess)
    case (GameLogic.FairySF(), FEN.FairySF(fen))
      => fairysf.format.Forsyth.<<(fen).map(Situation.FairySF)
    case _ => sys.error("Mismatched gamelogic types 15")
  }

  case class SituationPlus(situation: Situation, fullMoveNumber: Int) {

    def turns = fullMoveNumber * 2 - situation.color.fold(2, 1)

  }

  def <<<@(lib: GameLogic, variant: Variant, fen: FEN): Option[SituationPlus] =
    (lib, variant, fen) match {
      case (GameLogic.Draughts(), Variant.Draughts(variant), FEN.Draughts(fen))
        => draughts.format.Forsyth.<<<@(variant, fen).map(
          sp => SituationPlus(Situation.Draughts(sp.situation), sp.fullMoveNumber)
        )
      case (GameLogic.Chess(), Variant.Chess(variant), FEN.Chess(fen))
        => chess.format.Forsyth.<<<@(variant, fen).map(
          sp => SituationPlus(Situation.Chess(sp.situation), sp.fullMoveNumber)
        )
      case (GameLogic.FairySF(), Variant.FairySF(variant), FEN.FairySF(fen))
        => fairysf.format.Forsyth.<<<@(variant, fen).map(
          sp => SituationPlus(Situation.FairySF(sp.situation), sp.fullMoveNumber)
        )
      case _ => sys.error("Mismatched gamelogic types 16")
  }

  def <<<(lib: GameLogic, fen: FEN): Option[SituationPlus] = (lib, fen) match {
    case (GameLogic.Draughts(), FEN.Draughts(fen)) => draughts.format.Forsyth.<<<(fen).map(
      sp => SituationPlus(Situation.Draughts(sp.situation), sp.fullMoveNumber)
    )
    case (GameLogic.Chess(), FEN.Chess(fen))       => chess.format.Forsyth.<<<(fen).map(
      sp => SituationPlus(Situation.Chess(sp.situation), sp.fullMoveNumber)
    )
    case (GameLogic.FairySF(), FEN.FairySF(fen))   => fairysf.format.Forsyth.<<<(fen).map(
      sp => SituationPlus(Situation.FairySF(sp.situation), sp.fullMoveNumber)
    )
    case _ => sys.error("Mismatched gamelogic types 17")
  }

  def >>(lib: GameLogic, situation: Situation): FEN = >>(lib, SituationPlus(situation, 1))

  def >>(lib: GameLogic, parsed: SituationPlus): FEN = (lib, parsed.situation) match{
    case (GameLogic.Draughts(), Situation.Draughts(situation))
      => FEN.Draughts(draughts.format.Forsyth.>>(
        draughts.format.Forsyth.SituationPlus(situation, parsed.fullMoveNumber)
      ))
    case (GameLogic.Chess(), Situation.Chess(situation))
      => FEN.Chess(chess.format.Forsyth.>>(
        chess.format.Forsyth.SituationPlus(situation, parsed.fullMoveNumber)
      ))
    case (GameLogic.FairySF(), Situation.FairySF(situation))
      => FEN.FairySF(fairysf.format.Forsyth.>>(
        fairysf.format.Forsyth.SituationPlus(situation, parsed.fullMoveNumber)
      ))
    case _ => sys.error("Mismatched gamelogic types 19")
  }

  def >>(lib: GameLogic, game: Game): FEN = (lib, game) match {
    case (GameLogic.Draughts(), Game.Draughts(game))
      => FEN.Draughts(draughts.format.Forsyth.>>(game))
    case (GameLogic.Chess(), Game.Chess(game))
      => FEN.Chess(chess.format.Forsyth.>>(game))
    case (GameLogic.FairySF(), Game.FairySF(game))
      => FEN.FairySF(fairysf.format.Forsyth.>>(game))
    case _ => sys.error("Mismatched gamelogic types 20")
  }

  def exportBoard(lib: GameLogic, board: Board, algebraic: Boolean = false): String =
    (lib, board) match {
      case (GameLogic.Draughts(), Board.Draughts(board))
        => draughts.format.Forsyth.exportBoard(board, algebraic)
      case (GameLogic.Chess(), Board.Chess(board))
        => chess.format.Forsyth.exportBoard(board)
      case (GameLogic.FairySF(), Board.FairySF(board))
        => fairysf.format.Forsyth.exportBoard(board)
      case _ => sys.error("Mismatched gamelogic types 21")
  }

  def boardAndColor(lib: GameLogic, situation: Situation): String =
    boardAndColor(lib, situation.board, situation.color)

  private def boardAndColor(lib: GameLogic, board: Board, turnColor: Color): String =
    (lib, board) match {
      case (GameLogic.Draughts(), Board.Draughts(board))
        => draughts.format.Forsyth.boardAndColor(board, turnColor)
      case (GameLogic.Chess(), Board.Chess(board))
        => chess.format.Forsyth.boardAndColor(board, turnColor)
      case (GameLogic.FairySF(), Board.FairySF(board))
        => fairysf.format.Forsyth.boardAndColor(board, turnColor)
      case _ => sys.error("Mismatched gamelogic types 22")
  }

}
