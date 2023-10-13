package strategygames

import cats.data.Validated
import cats.implicits._

import strategygames.variant.Variant
import strategygames.format.{ FEN, Uci }

abstract class Game(
    val situation: Situation,
    val actions: VActions = Vector(),
    val clock: Option[Clock] = None,
    // We can look to remove 'plies' when draughts is converted to multiaction?
    val plies: Int = 0, // this was turns
    val turnCount: Int = 0,
    val startedAtTurn: Int = 0,
    val startPlayer: Player = Player.P1
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

  def applyUci(
      uci: Uci,
      metrics: MoveMetrics,
      finalSquare: Boolean = false,
      captures: Option[List[Pos]] = None,
      partialCaptures: Boolean = false
  ): Validated[String, (Game, MoveOrDrop)] =
    uci match {
      case Uci.ChessMove(uci)        =>
        apply(
          Pos.Chess(uci.orig),
          Pos.Chess(uci.dest),
          uci.promotion.map(Role.ChessPromotableRole),
          metrics
        ) map { case (ncg, move) =>
          ncg -> Left(move)
        }
      case Uci.DraughtsMove(uci)     =>
        apply(
          Pos.Draughts(uci.orig),
          Pos.Draughts(uci.dest),
          uci.promotion.map(Role.DraughtsPromotableRole),
          metrics,
          finalSquare,
          captures,
          partialCaptures
        ) map { case (ncg, move) =>
          ncg -> Left(move)
        }
      case Uci.FairySFMove(uci)      =>
        apply(
          Pos.FairySF(uci.orig),
          Pos.FairySF(uci.dest),
          uci.promotion.map(Role.FairySFPromotableRole),
          metrics
        ) map { case (ncg, move) =>
          ncg -> Left(move)
        }
      case Uci.SamuraiMove(uci)      =>
        apply(
          Pos.Samurai(uci.orig),
          Pos.Samurai(uci.dest),
          promotion = None,
          metrics
        ) map { case (ncg, move) =>
          ncg -> Left(move)
        }
      case Uci.TogyzkumalakMove(uci) =>
        apply(
          Pos.Togyzkumalak(uci.orig),
          Pos.Togyzkumalak(uci.dest),
          promotion = None,
          metrics
        ) map { case (ncg, move) =>
          ncg -> Left(move)
        }
      case Uci.ChessDrop(uci)        =>
        drop(
          Role.ChessRole(uci.role),
          Pos.Chess(uci.pos),
          metrics
        ) map { case (ncg, drop) =>
          ncg -> Right(drop)
        }
      case Uci.FairySFDrop(uci)      =>
        drop(
          Role.FairySFRole(uci.role),
          Pos.FairySF(uci.pos),
          metrics
        ) map { case (ncg, drop) =>
          ncg -> Right(drop)
        }
    }

  def drop(
      role: Role,
      pos: Pos,
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, Drop)]

  // Because I"m unsure how to properly write a single, generic copy
  // type signature, we're getting individual ones for how we use it.
  // TODO: figure out if we can properly make this generic
  def copy(clock: Option[Clock]): Game
  def copy(plies: Int, turnCount: Int, startedAtTurn: Int, startPlayer: Player): Game
  def copy(clock: Option[Clock], plies: Int, turnCount: Int, startedAtTurn: Int, startPlayer: Player): Game
  def copy(situation: Situation, plies: Int, turnCount: Int): Game
  def copy(situation: Situation): Game

  def player = situation.player

  def board = situation.board

  // Aka Fullmove number (in Forsyth-Edwards Notation):
  // The number of the completed turns by each player ('full move')
  // It starts at 1, and is incremented after P2's move (turn)
  def fullTurnCount: Int = 1 + turnCount / 2

  // TODO: When draughts is converted to multimove and we are happy that
  // turns - startedAtTurn == actions.size then we could consider deprecating `val turns`
  // and having it calculated from actions.size. Would need to check end of turn status
  // def turnCount = turns - startedAtTurn
  // def plyCount = actions.map(_.size).sum

  def withTurns(p: Int, t: Int): Game

  // TODO: Again, unsafe until we figure out the better design.
  def toChess: chess.Game
  def toDraughts: draughts.DraughtsGame
  def toFairySF: fairysf.Game
  def toSamurai: samurai.Game
  def toTogyzkumalak: togyzkumalak.Game

}

object Game {

  final case class Chess(g: chess.Game)
      extends Game(
        Situation.Chess(g.situation),
        g.actions,
        g.clock,
        g.plies,
        g.turnCount,
        g.startedAtTurn,
        g.startPlayer
      ) {

    private def toChessPromotion(p: Option[PromotableRole]): Option[chess.PromotableRole] =
      p.map(_ match {
        case Role.ChessPromotableRole(p) => p
        case _                           => sys.error("Non-chess promotable role paired with chess objects")
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
      case _                                  => sys.error("Not passed Chess objects")
    }

    def apply(move: Move): Game = move match {
      case (Move.Chess(move)) => Chess(g.apply(move))
      case _                  => sys.error("Not passed Chess objects")
    }

    def apply(moveOrDrop: MoveOrDrop): Game =
      moveOrDrop.fold(
        apply,
        drop =>
          drop match {
            case (Drop.Chess(drop)) => Chess(g.applyDrop(drop))
            case _                  => sys.error("Not passed Chess objects")
          }
      )

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
      case _                                      => sys.error("Not passed Chess objects")
    }

    def copy(clock: Option[Clock]): Game =
      Chess(g.copy(clock = clock))

    def copy(plies: Int, turnCount: Int, startedAtTurn: Int, startPlayer: Player): Game =
      Chess(
        g.copy(
          plies = plies,
          turnCount = turnCount,
          startedAtTurn = startedAtTurn,
          startPlayer = startPlayer
        )
      )

    def copy(
        clock: Option[Clock],
        plies: Int,
        turnCount: Int,
        startedAtTurn: Int,
        startPlayer: Player
    ): Game =
      Chess(
        g.copy(
          clock = clock,
          plies = plies,
          turnCount = turnCount,
          startedAtTurn = startedAtTurn,
          startPlayer = startPlayer
        )
      )

    def copy(situation: Situation, plies: Int, turnCount: Int): Game = situation match {
      case Situation.Chess(situation) =>
        Chess(g.copy(situation = situation, plies = plies, turnCount = turnCount))
      case _                          =>
        sys.error("Unable to copy chess game with non-chess arguments")
    }
    def copy(situation: Situation): Game                             = situation match {
      case Situation.Chess(situation) => Chess(g.copy(situation = situation))
      case _                          => sys.error("Unable to copy chess game with non-chess arguments")
    }

    def withTurns(p: Int, t: Int): Game = Chess(g.withTurns(p, t))

    def toChess: chess.Game               = g
    def toDraughts: draughts.DraughtsGame = sys.error("Can't turn a chess game into a draughts game")
    def toFairySF: fairysf.Game           = sys.error("Can't turn a chess game into a fairysf game")
    def toSamurai: samurai.Game           = sys.error("Can't turn a chess game into a samurai game")
    def toTogyzkumalak: togyzkumalak.Game = sys.error("Can't turn a chess game into a togyzkumalak game")

  }

  final case class Draughts(g: draughts.DraughtsGame)
      extends Game(
        Situation.Draughts(g.situation),
        g.actions,
        g.clock,
        g.plies,
        g.turnCount,
        g.startedAtTurn,
        g.startPlayer
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
        case None           => None
      }

    private def toDraughtsPromotion(p: Option[PromotableRole]): Option[draughts.PromotableRole] =
      p.map(_ match {
        case Role.DraughtsPromotableRole(p) => p
        case _                              => sys.error("Non-draughts promotable role paired with draughts objects")
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
            Pos.Draughts(dest)
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

    def copy(clock: Option[Clock]): Game =
      Draughts(g.copy(clock = clock))

    def copy(plies: Int, turnCount: Int, startedAtTurn: Int, startPlayer: Player): Game =
      Draughts(
        g.copy(
          plies = plies,
          turnCount = turnCount,
          startedAtTurn = startedAtTurn,
          startPlayer = startPlayer
        )
      )

    def copy(
        clock: Option[Clock],
        plies: Int,
        turnCount: Int,
        startedAtTurn: Int,
        startPlayer: Player
    ): Game =
      Draughts(
        g.copy(
          clock = clock,
          plies = plies,
          turnCount = turnCount,
          startedAtTurn = startedAtTurn,
          startPlayer = startPlayer
        )
      )

    def copy(situation: Situation, plies: Int, turnCount: Int): Game = situation match {
      case Situation.Draughts(situation) =>
        Draughts(g.copy(situation = situation, plies = plies, turnCount = turnCount))
      case _                             =>
        sys.error("Unable to copy draughts game with non-draughts arguments")
    }
    def copy(situation: Situation): Game                             = situation match {
      case Situation.Draughts(situation) => Draughts(g.copy(situation = situation))
      case _                             => sys.error("Unable to copy draughts game with non-draughts arguments")
    }

    def withTurns(p: Int, t: Int): Game = Draughts(g.withTurns(p, t))

    def toChess: chess.Game               = sys.error("Can't turn a draughts game into a chess game")
    def toDraughts: draughts.DraughtsGame = g
    def toFairySF: fairysf.Game           = sys.error("Can't turn a draughts game into a fairysf game")
    def toSamurai: samurai.Game           = sys.error("Can't turn a draughts game into a samurai game")
    def toTogyzkumalak: togyzkumalak.Game = sys.error("Can't turn a draughts game into a togyzkumalak game")

  }

  final case class FairySF(g: fairysf.Game)
      extends Game(
        Situation.FairySF(g.situation),
        g.actions,
        g.clock,
        g.plies,
        g.turnCount,
        g.startedAtTurn,
        g.startPlayer
      ) {

    private def toFairySFPromotion(p: Option[PromotableRole]): Option[fairysf.PromotableRole] =
      p.map(_ match {
        case Role.FairySFPromotableRole(p) => p
        case _                             => sys.error("Non-fairysf promotable role paired with fairysf objects")
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
      case _                                      => sys.error("Not passed FairySF objects")
    }

    private def apply(move: Move): Game = move match {
      case (Move.FairySF(move)) => FairySF(g.apply(move))
      case _                    => sys.error("Not passed FairySF objects")
    }

    def apply(moveOrDrop: MoveOrDrop): Game =
      moveOrDrop.fold(
        apply,
        drop =>
          drop match {
            case (Drop.FairySF(drop)) => FairySF(g.applyDrop(drop))
            case _                    => sys.error("Not passed FairySF objects")
          }
      )

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
      case _                                          => sys.error("Not passed FairySF objects")
    }

    def copy(clock: Option[Clock]): Game =
      FairySF(g.copy(clock = clock))

    def copy(plies: Int, turnCount: Int, startedAtTurn: Int, startPlayer: Player): Game =
      FairySF(
        g.copy(
          plies = plies,
          turnCount = turnCount,
          startedAtTurn = startedAtTurn,
          startPlayer = startPlayer
        )
      )

    def copy(
        clock: Option[Clock],
        plies: Int,
        turnCount: Int,
        startedAtTurn: Int,
        startPlayer: Player
    ): Game =
      FairySF(
        g.copy(
          clock = clock,
          plies = plies,
          turnCount = turnCount,
          startedAtTurn = startedAtTurn,
          startPlayer = startPlayer
        )
      )

    def copy(situation: Situation, plies: Int, turnCount: Int): Game = situation match {
      case Situation.FairySF(situation) =>
        FairySF(g.copy(situation = situation, plies = plies, turnCount = turnCount))
      case _                            =>
        sys.error("Unable to copy fairysf game with non-fairysf arguments")
    }
    def copy(situation: Situation): Game                             = situation match {
      case Situation.FairySF(situation) => FairySF(g.copy(situation = situation))
      case _                            => sys.error("Unable to copy fairysf game with non-fairysf arguments")
    }

    def withTurns(p: Int, t: Int): Game = FairySF(g.withTurns(p, t))

    def toFairySF: fairysf.Game           = g
    def toChess: chess.Game               = sys.error("Can't turn a fairysf game into a chess game")
    def toDraughts: draughts.DraughtsGame = sys.error("Can't turn a fairysf game into a draughts game")
    def toSamurai: samurai.Game           = sys.error("Can't turn a fairysf game into a samurai game")
    def toTogyzkumalak: togyzkumalak.Game = sys.error("Can't turn a fairysf game into a togyzkumalak game")

  }

  final case class Samurai(g: samurai.Game)
      extends Game(
        Situation.Samurai(g.situation),
        g.actions,
        g.clock,
        g.plies,
        g.turnCount,
        g.startedAtTurn,
        g.startPlayer
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
      case (Pos.Samurai(orig), Pos.Samurai(dest)) =>
        g.apply(orig, dest, None, metrics)
          .toEither
          .map(t => (Samurai(t._1), Move.Samurai(t._2)))
          .toValidated
      case _                                      => sys.error("Not passed Samurai objects")
    }

    private def apply(move: Move): Game = move match {
      case (Move.Samurai(move)) => Samurai(g.apply(move))
      case _                    => sys.error("Not passed Samurai objects")
    }

    def apply(moveOrDrop: MoveOrDrop): Game =
      moveOrDrop.fold(apply, _ => sys.error("Samurai does not support drops"))

    def drop(
        role: Role,
        pos: Pos,
        metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, Drop)] = sys.error("Can't drop in Samurai")

    def copy(clock: Option[Clock]): Game =
      Samurai(g.copy(clock = clock))

    def copy(plies: Int, turnCount: Int, startedAtTurn: Int, startPlayer: Player): Game =
      Samurai(
        g.copy(
          plies = plies,
          turnCount = turnCount,
          startedAtTurn = startedAtTurn,
          startPlayer = startPlayer
        )
      )

    def copy(
        clock: Option[Clock],
        plies: Int,
        turnCount: Int,
        startedAtTurn: Int,
        startPlayer: Player
    ): Game =
      Samurai(
        g.copy(
          clock = clock,
          plies = plies,
          turnCount = turnCount,
          startedAtTurn = startedAtTurn,
          startPlayer = startPlayer
        )
      )

    def copy(situation: Situation, plies: Int, turnCount: Int): Game = situation match {
      case Situation.Samurai(situation) =>
        Samurai(g.copy(situation = situation, plies = plies, turnCount = turnCount))
      case _                            => sys.error("Unable to copy samurai game with non-samurai arguments")
    }
    def copy(situation: Situation): Game                             = situation match {
      case Situation.Samurai(situation) => Samurai(g.copy(situation = situation))
      case _                            => sys.error("Unable to copy samurai game with non-samurai arguments")
    }

    def withTurns(p: Int, t: Int): Game = Samurai(g.withTurns(p, t))

    def toFairySF: fairysf.Game           = sys.error("Can't turn a samurai game into a fairysf game")
    def toChess: chess.Game               = sys.error("Can't turn a samurai game into a chess game")
    def toDraughts: draughts.DraughtsGame = sys.error("Can't turn a samurai game into a draughts game")
    def toSamurai: samurai.Game           = g
    def toTogyzkumalak: togyzkumalak.Game = sys.error("Can't turn a samurai game into a togyzkumalak game")

  }

  final case class Togyzkumalak(g: togyzkumalak.Game)
      extends Game(
        Situation.Togyzkumalak(g.situation),
        g.actions,
        g.clock,
        g.plies,
        g.turnCount,
        g.startedAtTurn,
        g.startPlayer
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
      case (Pos.Togyzkumalak(orig), Pos.Togyzkumalak(dest)) =>
        g.apply(orig, dest, None, metrics)
          .toEither
          .map(t => (Togyzkumalak(t._1), Move.Togyzkumalak(t._2)))
          .toValidated
      case _                                                => sys.error("Not passed Togyzkumalak objects")
    }

    private def apply(move: Move): Game = move match {
      case (Move.Togyzkumalak(move)) => Togyzkumalak(g.apply(move))
      case _                         => sys.error("Not passed Togyzkumalak objects")
    }

    def apply(moveOrDrop: MoveOrDrop): Game =
      moveOrDrop.fold(apply, _ => sys.error("Togyzkumalak does not support drops"))

    def drop(
        role: Role,
        pos: Pos,
        metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, Drop)] = sys.error("Can't drop in Togyzkumalak")

    def copy(clock: Option[Clock]): Game =
      Togyzkumalak(g.copy(clock = clock))

    def copy(plies: Int, turnCount: Int, startedAtTurn: Int, startPlayer: Player): Game =
      Togyzkumalak(
        g.copy(plies = plies, turnCount = turnCount, startedAtTurn = startedAtTurn, startPlayer = startPlayer)
      )

    def copy(
        clock: Option[Clock],
        plies: Int,
        turnCount: Int,
        startedAtTurn: Int,
        startPlayer: Player
    ): Game =
      Togyzkumalak(
        g.copy(
          clock = clock,
          plies = plies,
          turnCount = turnCount,
          startedAtTurn = startedAtTurn,
          startPlayer = startPlayer
        )
      )

    def copy(situation: Situation, plies: Int, turnCount: Int): Game = situation match {
      case Situation.Togyzkumalak(situation) =>
        Togyzkumalak(g.copy(situation = situation, plies = plies, turnCount = turnCount))
      case _                                 =>
        sys.error("Unable to copy togyzkumalak game with non-togyzkumalak arguments")
    }
    def copy(situation: Situation): Game                             = situation match {
      case Situation.Togyzkumalak(situation) => Togyzkumalak(g.copy(situation = situation))
      case _                                 => sys.error("Unable to copy togyzkumalak game with non-togyzkumalak arguments")
    }

    def withTurns(p: Int, t: Int): Game = Togyzkumalak(g.withTurns(p, t))

    def toFairySF: fairysf.Game           = sys.error("Can't turn a togyzkumalak game into a fairysf game")
    def toChess: chess.Game               = sys.error("Can't turn a togyzkumalak game into a chess game")
    def toDraughts: draughts.DraughtsGame = sys.error("Can't turn a togyzkumalak game into a draughts game")
    def toSamurai: samurai.Game           = sys.error("Can't turn a togyzkumalak game into a samurai game")
    def toTogyzkumalak: togyzkumalak.Game = g

  }

  def apply(
      lib: GameLogic,
      situation: Situation,
      actions: Vector[Vector[String]] = Vector(),
      clock: Option[Clock] = None,
      plies: Int = 0,
      turnCount: Int = 0,
      startedAtTurn: Int = 0,
      startPlayer: Player = Player.P1
  ): Game = (lib, situation) match {
    case (GameLogic.Draughts(), Situation.Draughts(situation))         =>
      Draughts(draughts.DraughtsGame(situation, actions, clock, plies, turnCount, startedAtTurn, startPlayer))
    case (GameLogic.Chess(), Situation.Chess(situation))               =>
      Chess(chess.Game(situation, actions, clock, plies, turnCount, startedAtTurn, startPlayer))
    case (GameLogic.FairySF(), Situation.FairySF(situation))           =>
      FairySF(fairysf.Game(situation, actions, clock, plies, turnCount, startedAtTurn, startPlayer))
    case (GameLogic.Samurai(), Situation.Samurai(situation))           =>
      Samurai(samurai.Game(situation, actions, clock, plies, turnCount, startedAtTurn, startPlayer))
    case (GameLogic.Togyzkumalak(), Situation.Togyzkumalak(situation)) =>
      Togyzkumalak(togyzkumalak.Game(situation, actions, clock, plies, turnCount, startedAtTurn, startPlayer))
    case _                                                             => sys.error("Mismatched gamelogic types 32")
  }

  def apply(lib: GameLogic, variant: Variant): Game = (lib, variant) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant))         =>
      Draughts(draughts.DraughtsGame.apply(variant))
    case (GameLogic.Chess(), Variant.Chess(variant))               =>
      Chess(chess.Game.apply(variant))
    case (GameLogic.FairySF(), Variant.FairySF(variant))           =>
      FairySF(fairysf.Game.apply(variant))
    case (GameLogic.Samurai(), Variant.Samurai(variant))           =>
      Samurai(samurai.Game.apply(variant))
    case (GameLogic.Togyzkumalak(), Variant.Togyzkumalak(variant)) =>
      Togyzkumalak(togyzkumalak.Game.apply(variant))
    case _                                                         =>
      sys.error("Mismatched gamelogic types 33")
  }

  def apply(lib: GameLogic, variant: Option[Variant], fen: Option[FEN]): Game = lib match {
    case GameLogic.Draughts()     =>
      Draughts(draughts.DraughtsGame.apply(variant.map(_.toDraughts), fen.map(_.toDraughts)))
    case GameLogic.Chess()        =>
      Chess(chess.Game.apply(variant.map(_.toChess), fen.map(_.toChess)))
    case GameLogic.FairySF()      =>
      FairySF(fairysf.Game.apply(variant.map(_.toFairySF), fen.map(_.toFairySF)))
    case GameLogic.Samurai()      =>
      Samurai(samurai.Game.apply(variant.map(_.toSamurai), fen.map(_.toSamurai)))
    case GameLogic.Togyzkumalak() =>
      Togyzkumalak(togyzkumalak.Game.apply(variant.map(_.toTogyzkumalak), fen.map(_.toTogyzkumalak)))
    case _                        => sys.error("Mismatched gamelogic types 36")
  }

  def wrap(g: chess.Game)            = Chess(g)
  def wrap(g: draughts.DraughtsGame) = Draughts(g)
  def wrap(g: fairysf.Game)          = FairySF(g)
  def wrap(g: samurai.Game)          = Samurai(g)
  def wrap(g: togyzkumalak.Game)     = Togyzkumalak(g)

}
