package strategygames.format

import strategygames._
import strategygames.variant.Variant

/** Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  */
object Forsyth {

  def initial(lib: GameLogic): FEN = lib match {
    case GameLogic.Draughts()     => FEN.Draughts(draughts.format.Forsyth.initial)
    case GameLogic.Chess()        => FEN.Chess(chess.format.Forsyth.initial)
    case GameLogic.FairySF()      => FEN.FairySF(fairysf.format.Forsyth.initial)
    case GameLogic.Samurai()      => FEN.Samurai(samurai.format.Forsyth.initial)
    case GameLogic.Togyzkumalak() => FEN.Togyzkumalak(togyzkumalak.format.Forsyth.initial)
    case GameLogic.Go()           => FEN.Go(go.format.Forsyth.initial)
    case GameLogic.Backgammon()   => FEN.Backgammon(backgammon.format.Forsyth.initial)
  }

  def <<@(lib: GameLogic, variant: Variant, fen: FEN): Option[Situation] =
    (lib, variant, fen) match {
      case (GameLogic.Draughts(), Variant.Draughts(variant), FEN.Draughts(fen))             =>
        draughts.format.Forsyth.<<@(variant, fen).map(Situation.Draughts)
      case (GameLogic.Chess(), Variant.Chess(variant), FEN.Chess(fen))                      =>
        chess.format.Forsyth.<<@(variant, fen).map(Situation.Chess)
      case (GameLogic.FairySF(), Variant.FairySF(variant), FEN.FairySF(fen))                =>
        fairysf.format.Forsyth.<<@(variant, fen).map(Situation.FairySF)
      case (GameLogic.Samurai(), Variant.Samurai(variant), FEN.Samurai(fen))                =>
        samurai.format.Forsyth.<<@(variant, fen).map(Situation.Samurai)
      case (GameLogic.Togyzkumalak(), Variant.Togyzkumalak(variant), FEN.Togyzkumalak(fen)) =>
        togyzkumalak.format.Forsyth.<<@(variant, fen).map(Situation.Togyzkumalak)
      case (GameLogic.Go(), Variant.Go(variant), FEN.Go(fen))                               =>
        go.format.Forsyth.<<@(variant, fen).map(Situation.Go)
      case (GameLogic.Backgammon(), Variant.Backgammon(variant), FEN.Backgammon(fen))       =>
        backgammon.format.Forsyth.<<@(variant, fen).map(Situation.Backgammon)
      case _                                                                                => sys.error("Mismatched gamelogic types 14")
    }

  def <<(lib: GameLogic, fen: FEN): Option[Situation] = (lib, fen) match {
    case (GameLogic.Draughts(), FEN.Draughts(fen))         => draughts.format.Forsyth.<<(fen).map(Situation.Draughts)
    case (GameLogic.Chess(), FEN.Chess(fen))               => chess.format.Forsyth.<<(fen).map(Situation.Chess)
    case (GameLogic.FairySF(), FEN.FairySF(fen))           => fairysf.format.Forsyth.<<(fen).map(Situation.FairySF)
    case (GameLogic.Samurai(), FEN.Samurai(fen))           => samurai.format.Forsyth.<<(fen).map(Situation.Samurai)
    case (GameLogic.Togyzkumalak(), FEN.Togyzkumalak(fen)) =>
      togyzkumalak.format.Forsyth.<<(fen).map(Situation.Togyzkumalak)
    case (GameLogic.Go(), FEN.Go(fen))                     => go.format.Forsyth.<<(fen).map(Situation.Go)
    case (GameLogic.Backgammon(), FEN.Backgammon(fen))     =>
      backgammon.format.Forsyth.<<(fen).map(Situation.Backgammon)
    case _                                                 => sys.error("Mismatched gamelogic types 15")
  }

  case class SituationPlus(situation: Situation, fullTurnCount: Int) {

    def turnCount = fullTurnCount * 2 - situation.player.fold(2, 1)
    def plies     = turnCount

  }

  def <<<@(lib: GameLogic, variant: Variant, fen: FEN): Option[SituationPlus] =
    (lib, variant, fen) match {
      case (GameLogic.Draughts(), Variant.Draughts(variant), FEN.Draughts(fen))             =>
        draughts.format.Forsyth
          .<<<@(variant, fen)
          .map(sp => SituationPlus(Situation.Draughts(sp.situation), sp.fullTurnCount))
      case (GameLogic.Chess(), Variant.Chess(variant), FEN.Chess(fen))                      =>
        chess.format.Forsyth
          .<<<@(variant, fen)
          .map(sp => SituationPlus(Situation.Chess(sp.situation), sp.fullTurnCount))
      case (GameLogic.FairySF(), Variant.FairySF(variant), FEN.FairySF(fen))                =>
        fairysf.format.Forsyth
          .<<<@(variant, fen)
          .map(sp => SituationPlus(Situation.FairySF(sp.situation), sp.fullTurnCount))
      case (GameLogic.Samurai(), Variant.Samurai(variant), FEN.Samurai(fen))                =>
        samurai.format.Forsyth
          .<<<@(variant, fen)
          .map(sp => SituationPlus(Situation.Samurai(sp.situation), sp.fullTurnCount))
      case (GameLogic.Togyzkumalak(), Variant.Togyzkumalak(variant), FEN.Togyzkumalak(fen)) =>
        togyzkumalak.format.Forsyth
          .<<<@(variant, fen)
          .map(sp => SituationPlus(Situation.Togyzkumalak(sp.situation), sp.fullTurnCount))
      case (GameLogic.Go(), Variant.Go(variant), FEN.Go(fen))                               =>
        go.format.Forsyth
          .<<<@(variant, fen)
          .map(sp => SituationPlus(Situation.Go(sp.situation), sp.fullTurnCount))
      case (GameLogic.Backgammon(), Variant.Backgammon(variant), FEN.Backgammon(fen))       =>
        backgammon.format.Forsyth
          .<<<@(variant, fen)
          .map(sp => SituationPlus(Situation.Backgammon(sp.situation), sp.fullTurnCount))

      case _ => sys.error("Mismatched gamelogic types 16")
    }

  def <<<(lib: GameLogic, fen: FEN): Option[SituationPlus] = (lib, fen) match {
    case (GameLogic.Draughts(), FEN.Draughts(fen))         =>
      draughts.format.Forsyth
        .<<<(fen)
        .map(sp => SituationPlus(Situation.Draughts(sp.situation), sp.fullTurnCount))
    case (GameLogic.Chess(), FEN.Chess(fen))               =>
      chess.format.Forsyth
        .<<<(fen)
        .map(sp => SituationPlus(Situation.Chess(sp.situation), sp.fullTurnCount))
    case (GameLogic.FairySF(), FEN.FairySF(fen))           =>
      fairysf.format.Forsyth
        .<<<(fen)
        .map(sp => SituationPlus(Situation.FairySF(sp.situation), sp.fullTurnCount))
    case (GameLogic.Samurai(), FEN.Samurai(fen))           =>
      samurai.format.Forsyth
        .<<<(fen)
        .map(sp => SituationPlus(Situation.Samurai(sp.situation), sp.fullTurnCount))
    case (GameLogic.Togyzkumalak(), FEN.Togyzkumalak(fen)) =>
      togyzkumalak.format.Forsyth
        .<<<(fen)
        .map(sp => SituationPlus(Situation.Togyzkumalak(sp.situation), sp.fullTurnCount))
    case (GameLogic.Go(), FEN.Go(fen))                     =>
      go.format.Forsyth
        .<<<(fen)
        .map(sp => SituationPlus(Situation.Go(sp.situation), sp.fullTurnCount))
    case (GameLogic.Backgammon(), FEN.Backgammon(fen))     =>
      backgammon.format.Forsyth
        .<<<(fen)
        .map(sp => SituationPlus(Situation.Backgammon(sp.situation), sp.fullTurnCount))
    case _                                                 => sys.error("Mismatched gamelogic types 17")
  }

  def >>(lib: GameLogic, situation: Situation): FEN = >>(lib, SituationPlus(situation, 1))

  def >>(lib: GameLogic, parsed: SituationPlus): FEN = (lib, parsed.situation) match {
    case (GameLogic.Draughts(), Situation.Draughts(situation))         =>
      FEN.Draughts(
        draughts.format.Forsyth.>>(
          draughts.format.Forsyth.SituationPlus(situation, parsed.fullTurnCount)
        )
      )
    case (GameLogic.Chess(), Situation.Chess(situation))               =>
      FEN.Chess(
        chess.format.Forsyth.>>(
          chess.format.Forsyth.SituationPlus(situation, parsed.fullTurnCount)
        )
      )
    case (GameLogic.FairySF(), Situation.FairySF(situation))           =>
      FEN.FairySF(
        fairysf.format.Forsyth.>>(
          fairysf.format.Forsyth.SituationPlus(situation, parsed.fullTurnCount)
        )
      )
    case (GameLogic.Samurai(), Situation.Samurai(situation))           =>
      FEN.Samurai(
        samurai.format.Forsyth.>>(
          samurai.format.Forsyth.SituationPlus(situation, parsed.fullTurnCount)
        )
      )
    case (GameLogic.Togyzkumalak(), Situation.Togyzkumalak(situation)) =>
      FEN.Togyzkumalak(
        togyzkumalak.format.Forsyth.>>(
          togyzkumalak.format.Forsyth.SituationPlus(situation, parsed.fullTurnCount)
        )
      )
    case (GameLogic.Go(), Situation.Go(situation))                     =>
      FEN.Go(
        go.format.Forsyth.>>(
          go.format.Forsyth.SituationPlus(situation, parsed.fullTurnCount)
        )
      )
    case (GameLogic.Backgammon(), Situation.Backgammon(situation))     =>
      FEN.Backgammon(
        backgammon.format.Forsyth.>>(
          backgammon.format.Forsyth.SituationPlus(situation, parsed.fullTurnCount)
        )
      )
    case _                                                             => sys.error("Mismatched gamelogic types 19")
  }

  def >>(lib: GameLogic, game: Game): FEN = (lib, game) match {
    case (GameLogic.Draughts(), Game.Draughts(game))         => FEN.Draughts(draughts.format.Forsyth.>>(game))
    case (GameLogic.Chess(), Game.Chess(game))               => FEN.Chess(chess.format.Forsyth.>>(game))
    case (GameLogic.FairySF(), Game.FairySF(game))           => FEN.FairySF(fairysf.format.Forsyth.>>(game))
    case (GameLogic.Samurai(), Game.Samurai(game))           => FEN.Samurai(samurai.format.Forsyth.>>(game))
    case (GameLogic.Togyzkumalak(), Game.Togyzkumalak(game)) =>
      FEN.Togyzkumalak(togyzkumalak.format.Forsyth.>>(game))
    case (GameLogic.Go(), Game.Go(game))                     => FEN.Go(go.format.Forsyth.>>(game))
    case (GameLogic.Backgammon(), Game.Backgammon(game))     =>
      FEN.Backgammon(backgammon.format.Forsyth.>>(game))
    case _                                                   => sys.error("Mismatched gamelogic types 20")
  }

  def exportBoard(lib: GameLogic, board: Board, algebraic: Boolean = false): String =
    (lib, board) match {
      case (GameLogic.Draughts(), Board.Draughts(board))         =>
        draughts.format.Forsyth.exportBoard(board, algebraic)
      case (GameLogic.Chess(), Board.Chess(board))               =>
        chess.format.Forsyth.exportBoard(board)
      case (GameLogic.FairySF(), Board.FairySF(board))           =>
        fairysf.format.Forsyth.exportBoard(board)
      case (GameLogic.Samurai(), Board.Samurai(board))           =>
        samurai.format.Forsyth.exportBoard(board)
      case (GameLogic.Togyzkumalak(), Board.Togyzkumalak(board)) =>
        togyzkumalak.format.Forsyth.exportBoard(board)
      case (GameLogic.Go(), Board.Go(board))                     =>
        go.format.Forsyth.exportBoard(board)
      case (GameLogic.Backgammon(), Board.Backgammon(board))     =>
        backgammon.format.Forsyth.exportBoard(board)
      case _                                                     => sys.error("Mismatched gamelogic types 21")
    }

  def boardAndPlayer(lib: GameLogic, situation: Situation): String =
    boardAndPlayer(lib, situation.board, situation.player)

  private def boardAndPlayer(lib: GameLogic, board: Board, turnPlayer: Player): String =
    (lib, board) match {
      case (GameLogic.Draughts(), Board.Draughts(board))         =>
        draughts.format.Forsyth.boardAndPlayer(board, turnPlayer)
      case (GameLogic.Chess(), Board.Chess(board))               =>
        chess.format.Forsyth.boardAndPlayer(board, turnPlayer)
      case (GameLogic.FairySF(), Board.FairySF(board))           =>
        fairysf.format.Forsyth.boardAndPlayer(board, turnPlayer)
      case (GameLogic.Samurai(), Board.Samurai(board))           =>
        samurai.format.Forsyth.boardAndPlayer(board, turnPlayer)
      case (GameLogic.Togyzkumalak(), Board.Togyzkumalak(board)) =>
        togyzkumalak.format.Forsyth.boardAndPlayer(board, turnPlayer)
      case (GameLogic.Go(), Board.Go(board))                     =>
        go.format.Forsyth.boardAndPlayer(board, turnPlayer)
      case (GameLogic.Backgammon(), Board.Backgammon(board))     =>
        backgammon.format.Forsyth.boardAndPlayer(board, turnPlayer)
      case _                                                     => sys.error("Mismatched gamelogic types 22")
    }

}
