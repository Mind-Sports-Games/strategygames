package strategygames

import cats.data.Validated
import cats.implicits._

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
  def apply(moveOrdrop: MoveOrDrop): Game

  def apply(
      orig: Pos,
      dest: Pos,
      promotion: Option[PromotableRole] = None,
      metrics: MoveMetrics = MoveMetrics(),
      finalSquare: Boolean = false,
      captures: Option[List[Pos]] = None,
      partialCaptures: Boolean = false
  ): Validated[String, (Game, Move)]

  // Because I"m unsure how to properly write a single, generic copy
  // type signature, we're getting individual ones for how we use it.
  // TODO: figure out if we can properly make this generic
  def copy(clock: Option[Clock]): Game
  def copy(turns: Int, startedAtTurn: Int): Game
  def copy(clock: Option[Clock], turns: Int, startedAtTurn: Int): Game
  def copy(situation: Situation, turns: Int): Game
  def copy(situation: Situation): Game

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

  // TODO: Again, unsafe until we figure out the better design.
  def toChess: chess.Game
  def toDraughts: draughts.DraughtsGame

}

object Game {

  private def toChessPromotion(p: Option[PromotableRole]): Option[chess.PromotableRole] =
    p.map(_ match {
      case Role.ChessPromotableRole(p) => p
      case _ => sys.error("Non-chess promotable role paired with chess objects")
    })

  final case class Chess(g: chess.Game)
      extends Game(
        Situation.Chess(g.situation),
        g.pgnMoves,
        g.clock,
        g.turns,
        g.startedAtTurn
      ) {

    def apply(
        orig: Pos,
        dest: Pos,
        promotion: Option[PromotableRole] = None,
        metrics: MoveMetrics = MoveMetrics(),
        finalSquare: Boolean = false,
        captures: Option[List[Pos]] = None,
        partialCaptures: Boolean = false
    ): Validated[String, (Game, Move)] = (orig, dest) match {
      case (Pos.Chess(orig), Pos.Chess(dest)) =>
        g.apply(orig, dest, toChessPromotion(promotion), metrics)
          .toEither
          .map(t => (Chess(t._1), Move.Chess(t._2)))
          .toValidated
      case _ => sys.error("Not passed Chess objects")
    }

    def apply(move: Move): Game = move match {
      case (Move.Chess(move)) => Chess(g.apply(move))
      case _                  => sys.error("Not passed Chess objects")
    }

    def apply(moveOrDrop: MoveOrDrop): Game =
      moveOrDrop.fold(apply, drop => Chess(g.applyDrop(drop)))

    def apply(uci: Uci.Move): Validated[String, (Game, Move)] = uci match {
      case Uci.ChessMove(uci) => g.apply(uci).toEither.map(t => (Chess(t._1), Move.Chess(t._2))).toValidated
      case _                  => sys.error("Not passed Chess objects")
    }

    def copy(clock: Option[Clock]): Game = Chess(g.copy(clock = clock))
    def copy(turns: Int, startedAtTurn: Int): Game = Chess(
      g.copy(turns = turns, startedAtTurn = startedAtTurn)
    )
    def copy(clock: Option[Clock], turns: Int, startedAtTurn: Int): Game = Chess(
      g.copy(clock = clock, turns = turns, startedAtTurn = startedAtTurn)
    )

    def copy(situation: Situation, turns: Int): Game = situation match {
      case Situation.Chess(situation) => Chess(g.copy(situation=situation, turns=turns))
      case _ => sys.error("Unable to copy chess game with non-chess arguments")
    }
    def copy(situation: Situation): Game = situation match {
      case Situation.Chess(situation) => Chess(g.copy(situation=situation))
      case _ => sys.error("Unable to copy chess game with non-chess arguments")
    }

    def isStandardInit: Boolean = g.isStandardInit

    def withBoard(b: Board): Game = b match {
      case (Board.Chess(b)) => Chess(g.withBoard(b))
      case _                => sys.error("Not passed Chess objects")
    }

    def withPlayer(c: Color): Game = Chess(g.withPlayer(c))

    def withTurns(t: Int): Game = Chess(g.withTurns(t))

    def toChess: chess.Game = g
    def toDraughts: draughts.DraughtsGame = sys.error("Can't turn a chess game into a draughts game")

  }

  final case class Draughts(g: draughts.DraughtsGame)
      extends Game(
        Situation.Draughts(g.situation),
        g.pdnMoves,
        g.clock,
        g.turns,
        g.startedAtTurn
      ) {

    private def draughtsCaptures(captures: Option[List[Pos]]): Option[List[draughts.Pos]] =
      captures match {
        case Some(captures) =>
          Some(
            captures.flatMap(c =>
              c match {
                case Pos.Draughts(c) => Some(c)
                case _               => None
              }
            )
          )
        case None => None
      }

    def apply(
        orig: Pos,
        dest: Pos,
        promotion: Option[PromotableRole] = None,
        metrics: MoveMetrics = MoveMetrics(),
        finalSquare: Boolean = false,
        captures: Option[List[Pos]] = None,
        partialCaptures: Boolean = false
    ): Validated[String, (Game, Move)] = (orig, dest, promotion) match {
      case (
            Pos.Draughts(orig),
            Pos.Draughts(dest),
            Some(Role.DraughtsPromotableRole(promotion))
          ) =>
        g.apply(
          orig,
          dest,
          Some(promotion),
          metrics,
          finalSquare,
          draughtsCaptures(captures),
          partialCaptures
        ).toEither
          .map(t => (Draughts(t._1), Move.Draughts(t._2)))
          .toValidated
      case _ => sys.error("Not passed Chess objects")
    }

    def apply(move: Move): Game = move match {
      case (Move.Draughts(move)) => Draughts(g.apply(move))
      case _                     => sys.error("Not passed Draughts objects")
    }

    def apply(moveOrDrop: MoveOrDrop): Game =
      moveOrDrop.fold(apply, _ => sys.error("Draughts doesn't support drops"))

    def apply(uci: Uci.Move): Validated[String, (Game, Move)] = uci match {
      case Uci.DraughtsMove(uci) =>
        g.apply(uci).toEither.map(t => (Draughts(t._1), Move.Draughts(t._2))).toValidated
      case _ => sys.error("Not passed Draughts objects")
    }

    def copy(clock: Option[Clock]): Game = Draughts(g.copy(clock = clock))
    def copy(turns: Int, startedAtTurn: Int): Game = Draughts(
      g.copy(turns = turns, startedAtTurn = startedAtTurn)
    )
    def copy(clock: Option[Clock], turns: Int, startedAtTurn: Int): Game = Draughts(
      g.copy(clock = clock, turns = turns, startedAtTurn = startedAtTurn)
    )
    def copy(situation: Situation, turns: Int): Game = situation match {
      case Situation.Draughts(situation) => Draughts(g.copy(situation=situation, turns=turns))
      case _ => sys.error("Unable to copy draughts game with non-draughts arguments")
    }
    def copy(situation: Situation): Game = situation match {
      case Situation.Draughts(situation) => Draughts(g.copy(situation=situation))
      case _ => sys.error("Unable to copy draughts game with non-draughts arguments")
    }

    def isStandardInit: Boolean = g.isStandardInit

    def withBoard(b: Board): Game = b match {
      case (Board.Draughts(b)) => Draughts(g.withBoard(b))
      case _                   => sys.error("Not passed Draughts objects")
    }

    def withPlayer(c: Color): Game = Draughts(g.withPlayer(c))

    def withTurns(t: Int): Game = Draughts(g.withTurns(t))

    def toChess: chess.Game = sys.error("Can't turn a draughts game into a chess game")
    def toDraughts: draughts.DraughtsGame = g

  }

  def apply(
      lib: GameLib,
      situation: Situation,
      pgnMoves: Vector[String] = Vector(),
      clock: Option[Clock] = None,
      turns: Int = 0, // plies
      startedAtTurn: Int = 0
  ): Game = (lib, situation) match {
    case (GameLib.Draughts(), Situation.Draughts(situation)) =>
      Draughts(draughts.DraughtsGame(situation, pgnMoves, clock, turns, startedAtTurn))
    case (GameLib.Chess(), Situation.Chess(situation)) =>
      Chess(chess.Game(situation, pgnMoves, clock, turns, startedAtTurn))
    case _ => sys.error("Mismatched gamelib types 32")
  }

  def apply(lib: GameLib, variant: Variant): Game = (lib, variant) match {
    case (GameLib.Draughts(), Variant.Draughts(variant)) => Draughts(draughts.DraughtsGame.apply(variant))
    case (GameLib.Chess(), Variant.Chess(variant))       => Chess(chess.Game.apply(variant))
    case _                                               => sys.error("Mismatched gamelib types 33")
  }

  def apply(lib: GameLib, board: Board): Game = (lib, board) match {
    case (GameLib.Draughts(), Board.Draughts(board)) => Draughts(draughts.DraughtsGame.apply(board))
    case (GameLib.Chess(), Board.Chess(board))       => Chess(chess.Game.apply(board))
    case _                                           => sys.error("Mismatched gamelib types 34")
  }

  def apply(lib: GameLib, board: Board, color: Color): Game = (lib, board) match {
    case (GameLib.Draughts(), Board.Draughts(board)) => Draughts(draughts.DraughtsGame.apply(board, color))
    case (GameLib.Chess(), Board.Chess(board))       => Chess(chess.Game.apply(board, color))
    case _                                           => sys.error("Mismatched gamelib types 35")
  }

  def apply(lib: GameLib, variant: Option[Variant], fen: Option[FEN]): Game =
    (lib, variant, fen) match {
      case (GameLib.Draughts(), Some(Variant.Draughts(variant)), Some(FEN.Draughts(fen))) =>
        Draughts(draughts.DraughtsGame.apply(Some(variant), Some(fen)))
      case (GameLib.Chess(), Some(Variant.Chess(variant)), Some(FEN.Chess(fen))) =>
        Chess(chess.Game.apply(Some(variant), Some(fen)))
      case _ => sys.error("Mismatched gamelib types 36")
    }

    def wrap(g: chess.Game) = Chess(g)
    def wrap(g: draughts.DraughtsGame) = Draughts(g)

}
