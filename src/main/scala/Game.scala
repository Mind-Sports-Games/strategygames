package strategygames

import cats.data.Validated

import strategygames.variant.Variant
import strategygames.format.{ FEN, Uci }

abstract class Game(
  val situation: Situation,
  val pgnMoves: Vector[String] = Vector(),
  val clock: Option[Clock] = None,
  val turns: Int = 0, // plies
  val startedAtTurn: Int = 0
) {

  def apply(move: Move): Game

  //def apply(uci: Uci.Move): Validated[String, (Game, Move)]

  def player = situation.color

  def board = situation.board

  def isStandardInit: Boolean

  def halfMoveClock: Int = board.history.halfMoveClock

  /** Fullmove number: The number of the full move.
    * It starts at 1, and is incremented after Black's move.
    */
  def fullMoveNumber: Int = 1 + turns / 2

  def moveString = s"$fullMoveNumber${player.fold(".", "...")}"

  def withBoard(b: Board): Game

  def updateBoard(f: Board => Board) = withBoard(f(board))

  def withPlayer(c: Color): Game

  def withTurns(t: Int): Game

}

object Game {

  final case class Chess(g: chess.Game) extends Game(
    Situation.Chess(g.situation),
    g.pgnMoves,
    g.clock,
    g.turns,
    g.startedAtTurn
  ) {

    def apply(move: Move): Game = move match {
      case (Move.Chess(move)) => Chess(g.apply(move))
      case _ => sys.error("Not passed Chess objects")
    }

    //TODO: need to convert?
    //def apply(uci: Uci.Move): Validated[String, (Game, Move)] = uci match {
    //  case (Uci.Move.Chess(uci)) => g.apply(uci)
    //  case _ => sys.error("Not passed Chess objects")
    //}

    def isStandardInit: Boolean = g.isStandardInit

    def withBoard(b: Board): Game = b match {
      case (Board.Chess(b)) => Chess(g.withBoard(b))
      case _ => sys.error("Not passed Chess objects")
    }

    def withPlayer(c: Color): Game = c match {
      case (Color.Chess(c)) => Chess(g.withPlayer(c))
      case _ => sys.error("Not passed Chess objects")
    }

    def withTurns(t: Int): Game = Chess(g.withTurns(t))

  }

  final case class Draughts(g: draughts.DraughtsGame) extends Game(
    Situation.Draughts(g.situation),
    g.pdnMoves,
    g.clock,
    g.turns,
    g.startedAtTurn
  ) {

    def apply(move: Move): Game = move match {
      case (Move.Draughts(move)) => Draughts(g.apply(move))
      case _ => sys.error("Not passed Draughts objects")
    }

    //TODO: need to convert?
    //def apply(uci: Uci.Move): Validated[String, (Game, Move)] = uci match {
    //  case (Uci.Move.Draughts(uci)) => g.apply(uci)
    //  case _ => sys.error("Not passed Draughts objects")
    //}

    def isStandardInit: Boolean = g.isStandardInit

    def withBoard(b: Board): Game = b match {
      case (Board.Draughts(b)) => Draughts(g.withBoard(b))
      case _ => sys.error("Not passed Draughts objects")
    }

    def withPlayer(c: Color): Game = c match {
      case (Color.Draughts(c)) => Draughts(g.withPlayer(c))
      case _ => sys.error("Not passed Draughts objects")
    }

    def withTurns(t: Int): Game = Draughts(g.withTurns(t))

  }

  def apply(lib: GameLib, variant: strategygames.variant.Variant): Game = (lib, variant) match {
    case (GameLib.Draughts(), Variant.Draughts(variant))
      => Draughts(draughts.DraughtsGame.apply(variant))
    case (GameLib.Chess(), Variant.Chess(variant))
      => Chess(chess.Game.apply(variant))
    case _ => sys.error("Mismatched gamelib types")
  }

  def apply(lib: GameLib, board: Board): Game = (lib, board) match {
    case (GameLib.Draughts(), Board.Draughts(board))
      => Draughts(draughts.DraughtsGame.apply(board))
    case (GameLib.Chess(), Board.Chess(board))
      => Chess(chess.Game.apply(board))
    case _ => sys.error("Mismatched gamelib types")
  }

  def apply(lib: GameLib, board: Board, color: Color): Game = (lib, board, color) match {
    case (GameLib.Draughts(), Board.Draughts(board), Color.Draughts(color))
      => Draughts(draughts.DraughtsGame.apply(board, color))
    case (GameLib.Chess(), Board.Chess(board), Color.Chess(color))
      => Chess(chess.Game.apply(board, color))
    case _ => sys.error("Mismatched gamelib types")
  }

}
