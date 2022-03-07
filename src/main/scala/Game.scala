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

  def drop(
      role: Role,
      pos: Pos,
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, Drop)]

  // Because I"m unsure how to properly write a single, generic copy
  // type signature, we're getting individual ones for how we use it.
  // TODO: figure out if we can properly make this generic
  def copy(clock: Option[Clock]): Game
  def copy(turns: Int, startedAtTurn: Int): Game
  def copy(clock: Option[Clock], turns: Int, startedAtTurn: Int): Game
  def copy(situation: Situation, turns: Int): Game
  def copy(situation: Situation): Game

  def player = situation.player

  def board = situation.board

  /** Fullmove number: The number of the full move.
    * It starts at 1, and is incremented after P2's move.
    */
  def fullMoveNumber: Int = 1 + turns / 2

  def withTurns(t: Int): Game

  // TODO: Again, unsafe until we figure out the better design.
  def toChess: chess.Game
  def toDraughts: draughts.DraughtsGame
  def toFairySF: fairysf.Game

}

object Game {

  final case class Chess(g: chess.Game)
      extends Game(
        Situation.Chess(g.situation),
        g.pgnMoves,
        g.clock,
        g.turns,
        g.startedAtTurn
      ) {

    private def toChessPromotion(p: Option[PromotableRole]): Option[chess.PromotableRole] =
      p.map(_ match {
        case Role.ChessPromotableRole(p) => p
        case _ => sys.error("Non-chess promotable role paired with chess objects")
      })

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
      moveOrDrop.fold(apply, drop => drop match {
        case (Drop.Chess(drop)) => Chess(g.applyDrop(drop))
        case _                  => sys.error("Not passed Chess objects")
      })

    def drop(
        role: Role,
        pos: Pos,
        metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, Drop)] = (role, pos) match {
      case (Role.ChessRole(role), Pos.Chess(pos)) =>
        g.drop(role, pos, metrics)
          .toEither
          .map(t => (Chess(t._1), Drop.Chess(t._2)))
          .toValidated
      case _ => sys.error("Not passed Chess objects")
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

    def withTurns(t: Int): Game = Chess(g.withTurns(t))

    def toChess: chess.Game = g
    def toDraughts: draughts.DraughtsGame = sys.error("Can't turn a chess game into a draughts game")
    def toFairySF: fairysf.Game = sys.error("Can't turn a chess game into a fairysf game")

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

    private def toDraughtsPromotion(p: Option[PromotableRole]): Option[draughts.PromotableRole] =
      p.map(_ match {
        case Role.DraughtsPromotableRole(p) => p
        case _ => sys.error("Non-draughts promotable role paired with draughts objects")
      })

    def apply(
        orig: Pos,
        dest: Pos,
        promotion: Option[PromotableRole] = None,
        metrics: MoveMetrics = MoveMetrics(),
        finalSquare: Boolean = false,
        captures: Option[List[Pos]] = None,
        partialCaptures: Boolean = false
    ): Validated[String, (Game, Move)] = (orig, dest) match {
      case (
            Pos.Draughts(orig),
            Pos.Draughts(dest),
          ) =>
        g.apply(
          orig,
          dest,
          toDraughtsPromotion(promotion),
          metrics,
          finalSquare,
          draughtsCaptures(captures),
          partialCaptures
        ).toEither
          .map(t => (Draughts(t._1), Move.Draughts(t._2)))
          .toValidated
      case _ => sys.error("Not passed Draughts objects")
    }

    def apply(move: Move): Game = move match {
      case (Move.Draughts(move)) => Draughts(g.apply(move))
      case _                     => sys.error("Not passed Draughts objects")
    }

    def apply(moveOrDrop: MoveOrDrop): Game =
      moveOrDrop.fold(apply, _ => sys.error("Draughts doesn't support drops"))

    def drop(
        role: Role,
        pos: Pos,
        metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, Drop)] = sys.error("Can't drop in draughts")

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

    def withTurns(t: Int): Game = Draughts(g.withTurns(t))

    def toChess: chess.Game = sys.error("Can't turn a draughts game into a chess game")
    def toDraughts: draughts.DraughtsGame = g
    def toFairySF: fairysf.Game = sys.error("Can't turn a draughts game into a fairysf game")

  }

  final case class FairySF(g: fairysf.Game)
      extends Game(
        Situation.FairySF(g.situation),
        g.pgnMoves,
        g.clock,
        g.turns,
        g.startedAtTurn
      ) {

    private def toFairySFPromotion(p: Option[PromotableRole]): Option[fairysf.PromotableRole] =
      p.map(_ match {
        case Role.FairySFPromotableRole(p) => p
        case _ => sys.error("Non-fairysf promotable role paired with fairysf objects")
      })

    def apply(
        orig: Pos,
        dest: Pos,
        promotion: Option[PromotableRole] = None,
        metrics: MoveMetrics = MoveMetrics(),
        finalSquare: Boolean = false,
        captures: Option[List[Pos]] = None,
        partialCaptures: Boolean = false
    ): Validated[String, (Game, Move)] = (orig, dest) match {
      case (Pos.FairySF(orig), Pos.FairySF(dest)) =>
        g.apply(orig, dest, toFairySFPromotion(promotion), metrics)
          .toEither
          .map(t => (FairySF(t._1), Move.FairySF(t._2)))
          .toValidated
      case _ => sys.error("Not passed FairySF objects")
    }

    private def apply(move: Move): Game = move match {
      case (Move.FairySF(move)) => FairySF(g.apply(move))
      case _                  => sys.error("Not passed FairySF objects")
    }

    def apply(moveOrDrop: MoveOrDrop): Game =
      moveOrDrop.fold(apply, drop => drop match {
        case (Drop.FairySF(drop)) => FairySF(g.applyDrop(drop))
        case _                    => sys.error("Not passed FairySF objects")
      })

    def drop(
        role: Role,
        pos: Pos,
        metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, Drop)] = (role, pos) match {
      case (Role.FairySFRole(role), Pos.FairySF(pos)) =>
        g.drop(role, pos, metrics)
          .toEither
          .map(t => (FairySF(t._1), Drop.FairySF(t._2)))
          .toValidated
      case _ => sys.error("Not passed FairySF objects")
    }

    def copy(clock: Option[Clock]): Game = FairySF(g.copy(clock = clock))
    def copy(turns: Int, startedAtTurn: Int): Game = FairySF(
      g.copy(turns = turns, startedAtTurn = startedAtTurn)
    )
    def copy(clock: Option[Clock], turns: Int, startedAtTurn: Int): Game = FairySF(
      g.copy(clock = clock, turns = turns, startedAtTurn = startedAtTurn)
    )

    def copy(situation: Situation, turns: Int): Game = situation match {
      case Situation.FairySF(situation) => FairySF(g.copy(situation=situation, turns=turns))
      case _ => sys.error("Unable to copy fairysf game with non-fairysf arguments")
    }
    def copy(situation: Situation): Game = situation match {
      case Situation.FairySF(situation) => FairySF(g.copy(situation=situation))
      case _ => sys.error("Unable to copy fairysf game with non-fairysf arguments")
    }

    def withTurns(t: Int): Game = FairySF(g.withTurns(t))

    def toFairySF: fairysf.Game = g
    def toChess: chess.Game = sys.error("Can't turn a fairysf game into a chess game")
    def toDraughts: draughts.DraughtsGame = sys.error("Can't turn a fairysf game into a draughts game")

  }

  def apply(
      lib: GameLogic,
      situation: Situation,
      pgnMoves: Vector[String] = Vector(),
      clock: Option[Clock] = None,
      turns: Int = 0, // plies
      startedAtTurn: Int = 0
  ): Game = (lib, situation) match {
    case (GameLogic.Draughts(), Situation.Draughts(situation)) =>
      Draughts(draughts.DraughtsGame(situation, pgnMoves, clock, turns, startedAtTurn))
    case (GameLogic.Chess(), Situation.Chess(situation)) =>
      Chess(chess.Game(situation, pgnMoves, clock, turns, startedAtTurn))
    case (GameLogic.FairySF(), Situation.FairySF(situation)) =>
      FairySF(fairysf.Game(situation, pgnMoves, clock, turns, startedAtTurn))
    case _ => sys.error("Mismatched gamelogic types 32")
  }

  def apply(lib: GameLogic, variant: Variant): Game = (lib, variant) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant)) => Draughts(draughts.DraughtsGame.apply(variant))
    case (GameLogic.Chess(), Variant.Chess(variant))       => Chess(chess.Game.apply(variant))
    case (GameLogic.FairySF(), Variant.FairySF(variant))   => FairySF(fairysf.Game.apply(variant))
    case _ => sys.error("Mismatched gamelogic types 33")
  }

  def apply(lib: GameLogic, variant: Option[Variant], fen: Option[FEN]): Game = lib match {
      case GameLogic.Draughts() =>
        Draughts(draughts.DraughtsGame.apply(variant.map(_.toDraughts), fen.map(_.toDraughts)))
      case GameLogic.Chess() =>
        Chess(chess.Game.apply(variant.map(_.toChess), fen.map(_.toChess)))
      case GameLogic.FairySF() =>
        FairySF(fairysf.Game.apply(variant.map(_.toFairySF), fen.map(_.toFairySF)))
      case _ => sys.error("Mismatched gamelogic types 36")
    }

    def wrap(g: chess.Game) = Chess(g)
    def wrap(g: draughts.DraughtsGame) = Draughts(g)
    def wrap(g: fairysf.Game) = FairySF(g)

}
