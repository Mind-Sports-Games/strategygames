package strategygames

import cats.data.Validated
import cats.implicits._

import strategygames.variant.Variant
import strategygames.format.{ FEN, Uci }

abstract class Game(
    val situation: Situation,
    val actionStrs: VActionStrs = Vector(),
    val clock: Option[ClockBase] = None,
    // TODO We can look to remove 'plies' when draughts is converted to multiaction?
    // as plies will then be the same as actionStrs.flatten.size + startedAtPly
    val plies: Int = 0, // this was turns
    val turnCount: Int = 0,
    val startedAtPly: Int = 0,
    val startedAtTurn: Int = 0
) {

  def apply(action: Action): Game

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
  ): Validated[String, (Game, Action)] =
    uci match {
      case Uci.ChessMove(uci)        =>
        apply(
          Pos.Chess(uci.orig),
          Pos.Chess(uci.dest),
          uci.promotion.map(Role.ChessPromotableRole),
          metrics
        ) map { case (ncg, move) =>
          ncg -> move
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
          ncg -> move
        }
      case Uci.FairySFMove(uci)      =>
        apply(
          Pos.FairySF(uci.orig),
          Pos.FairySF(uci.dest),
          uci.promotion.map(Role.FairySFPromotableRole),
          metrics
        ) map { case (ncg, move) =>
          ncg -> move
        }
      case Uci.SamuraiMove(uci)      =>
        apply(
          Pos.Samurai(uci.orig),
          Pos.Samurai(uci.dest),
          promotion = None,
          metrics
        ) map { case (ncg, move) =>
          ncg -> move
        }
      case Uci.TogyzkumalakMove(uci) =>
        apply(
          Pos.Togyzkumalak(uci.orig),
          Pos.Togyzkumalak(uci.dest),
          promotion = None,
          metrics
        ) map { case (ncg, move) =>
          ncg -> move
        }
      case Uci.BackgammonMove(uci)   =>
        apply(
          Pos.Backgammon(uci.orig),
          Pos.Backgammon(uci.dest),
          promotion = None,
          metrics
        ) map { case (ncg, move) =>
          ncg -> move
        }
      case Uci.ChessDrop(uci)        =>
        drop(
          Role.ChessRole(uci.role),
          Pos.Chess(uci.pos),
          metrics
        ) map { case (ncg, drop) =>
          ncg -> drop
        }
      case Uci.FairySFDrop(uci)      =>
        drop(
          Role.FairySFRole(uci.role),
          Pos.FairySF(uci.pos),
          metrics
        ) map { case (ncg, drop) =>
          ncg -> drop
        }
      case Uci.GoDrop(uci)           =>
        drop(
          Role.GoRole(uci.role),
          Pos.Go(uci.pos),
          metrics
        ) map { case (ncg, drop) =>
          ncg -> drop
        }
      case Uci.GoPass(_)             =>
        pass(
          metrics
        ) map { case (ncg, pass) =>
          ncg -> pass
        }
      case Uci.GoSelectSquares(uci)  =>
        selectSquares(
          uci.squares.map(Pos.Go(_)),
          metrics
        ) map { case (ncg, selectSquares) =>
          ncg -> selectSquares
        }
    }

  def drop(
      role: Role,
      pos: Pos,
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, Drop)]

  def pass(metrics: MoveMetrics = MoveMetrics()): Validated[String, (Game, Pass)]

  def selectSquares(
      squares: List[Pos],
      metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, SelectSquares)]

  def diceRoll(
    dice: List[Int],
    metrics: MoveMetrics = MoveMetrics()
  ): Validated[String, (Game, DiceRoll)]

  def randomizeDiceRoll: Option[DiceRoll]

  // Because I"m unsure how to properly write a single, generic copy
  // type signature, we're getting individual ones for how we use it.
  // TODO: figure out if we can properly make this generic
  def copy(clock: Option[ClockBase]): Game
  def copy(plies: Int, turnCount: Int, startedAtPly: Int, startedAtTurn: Int): Game
  def copy(clock: Option[ClockBase], plies: Int, turnCount: Int, startedAtPly: Int, startedAtTurn: Int): Game
  def copy(situation: Situation, plies: Int, turnCount: Int): Game
  def copy(situation: Situation): Game

  def hasJustSwitchedTurns: Boolean

  def player = situation.player

  def board = situation.board

  // Aka Fullmove number (in Forsyth-Edwards Notation):
  // The number of the completed turns by each player ('full move')
  // It starts at 1, and is incremented after P2's move (turn)
  def fullTurnCount: Int = 1 + turnCount / 2

  // TODO: When draughts is converted to multiaction and we are happy that
  // `turnCount - startedAtTurn == actionStrs.size` then we could consider deprecating
  // `val turnCount` and having it calculated from actionStrs.size.
  // However we would need to check end of turn status
  // def playedTurns = actionStrs.size// turnCount - startedAtTurn
  // def playedPlies = actionStrs.map(_.size).sum

  def withTurnsAndPlies(p: Int, t: Int): Game

  // TODO: Again, unsafe until we figure out the better design.
  def toChess: chess.Game
  def toDraughts: draughts.DraughtsGame
  def toFairySF: fairysf.Game
  def toSamurai: samurai.Game
  def toTogyzkumalak: togyzkumalak.Game
  def toGo: go.Game

}

object Game {

  final case class Chess(g: chess.Game)
      extends Game(
        Situation.Chess(g.situation),
        g.actionStrs,
        g.clock,
        g.plies,
        g.turnCount,
        g.startedAtPly,
        g.startedAtTurn
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

    def apply(action: Action): Game =
      action match {
        case (Move.Chess(move)) => Chess(g.apply(move))
        case (Drop.Chess(drop)) => Chess(g.applyDrop(drop))
        case _                  => sys.error("Not passed Chess objects")
      }

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

    def pass(metrics: MoveMetrics = MoveMetrics()): Validated[String, (Game, Pass)] =
      sys.error("Can't pass in chess")

    def selectSquares(
        squares: List[Pos],
        metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, SelectSquares)] =
      sys.error("Can't selectSquares in chess")

    def diceRoll(
      dice: List[Int],
      metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, DiceRoll)] =
      sys.error("Can't diceroll in chess")

    def randomizeDiceRoll: Option[DiceRoll] = None

    def copy(clock: Option[ClockBase]): Game =
      Chess(g.copy(clock = clock))

    def copy(plies: Int, turnCount: Int, startedAtPly: Int, startedAtTurn: Int): Game =
      Chess(
        g.copy(
          plies = plies,
          turnCount = turnCount,
          startedAtPly = startedAtPly,
          startedAtTurn = startedAtTurn
        )
      )

    def copy(
        clock: Option[ClockBase],
        plies: Int,
        turnCount: Int,
        startedAtPly: Int,
        startedAtTurn: Int
    ): Game =
      Chess(
        g.copy(
          clock = clock,
          plies = plies,
          turnCount = turnCount,
          startedAtPly = startedAtPly,
          startedAtTurn = startedAtTurn
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

    def hasJustSwitchedTurns: Boolean = g.hasJustSwitchedTurns

    def withTurnsAndPlies(p: Int, t: Int): Game = Chess(g.withTurnsAndPlies(p, t))

    def toChess: chess.Game               = g
    def toDraughts: draughts.DraughtsGame = sys.error("Can't turn a chess game into a draughts game")
    def toFairySF: fairysf.Game           = sys.error("Can't turn a chess game into a fairysf game")
    def toSamurai: samurai.Game           = sys.error("Can't turn a chess game into a samurai game")
    def toTogyzkumalak: togyzkumalak.Game = sys.error("Can't turn a chess game into a togyzkumalak game")
    def toGo: go.Game                     = sys.error("Can't turn a chess game into a go game")
    def toBackgammon: backgammon.Game     = sys.error("Can't turn a chess game into a backgammon game")

  }

  final case class Draughts(g: draughts.DraughtsGame)
      extends Game(
        Situation.Draughts(g.situation),
        g.actionStrs,
        g.clock,
        g.plies,
        g.turnCount,
        g.startedAtPly,
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

    def apply(action: Action): Game =
      action match {
        case (Move.Draughts(move)) => Draughts(g.apply(move))
        case _                     => sys.error("Not passed Draughts objects")
      }

    def drop(
        role: Role,
        pos: Pos,
        metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, Drop)] = sys.error("Can't drop in draughts")

    def pass(metrics: MoveMetrics = MoveMetrics()): Validated[String, (Game, Pass)] =
      sys.error("Can't pass in draughts")

    def selectSquares(
        squares: List[Pos],
        metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, SelectSquares)] =
      sys.error("Can't selectSquares in draughts")

    def diceRoll(
      dice: List[Int],
      metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, DiceRoll)] =
      sys.error("Can't diceroll in draughts")

    def randomizeDiceRoll: Option[DiceRoll] = None

    def copy(clock: Option[ClockBase]): Game =
      Draughts(g.copy(clock = clock))

    def copy(plies: Int, turnCount: Int, startedAtPly: Int, startedAtTurn: Int): Game =
      Draughts(
        g.copy(
          plies = plies,
          turnCount = turnCount,
          startedAtPly = startedAtPly,
          startedAtTurn = startedAtTurn
        )
      )

    def copy(
        clock: Option[ClockBase],
        plies: Int,
        turnCount: Int,
        startedAtPly: Int,
        startedAtTurn: Int
    ): Game =
      Draughts(
        g.copy(
          clock = clock,
          plies = plies,
          turnCount = turnCount,
          startedAtPly = startedAtPly,
          startedAtTurn = startedAtTurn
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

    def hasJustSwitchedTurns: Boolean = g.hasJustSwitchedTurns

    def withTurnsAndPlies(p: Int, t: Int): Game = Draughts(g.withTurnsAndPlies(p, t))

    def toChess: chess.Game               = sys.error("Can't turn a draughts game into a chess game")
    def toDraughts: draughts.DraughtsGame = g
    def toFairySF: fairysf.Game           = sys.error("Can't turn a draughts game into a fairysf game")
    def toSamurai: samurai.Game           = sys.error("Can't turn a draughts game into a samurai game")
    def toTogyzkumalak: togyzkumalak.Game = sys.error("Can't turn a draughts game into a togyzkumalak game")
    def toGo: go.Game                     = sys.error("Can't turn a draughts game into a go game")
    def toBackgammon: backgammon.Game     = sys.error("Can't turn a draughts game into a backgammon game")

  }

  final case class FairySF(g: fairysf.Game)
      extends Game(
        Situation.FairySF(g.situation),
        g.actionStrs,
        g.clock,
        g.plies,
        g.turnCount,
        g.startedAtPly,
        g.startedAtTurn
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

    def apply(action: Action): Game =
      action match {
        case (Move.FairySF(move)) => FairySF(g.apply(move))
        case (Drop.FairySF(drop)) => FairySF(g.applyDrop(drop))
        case _                    => sys.error("Not passed FairySF objects")
      }

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

    def pass(metrics: MoveMetrics = MoveMetrics()): Validated[String, (Game, Pass)] =
      sys.error("Can't pass in fairysf")

    def selectSquares(
        squares: List[Pos],
        metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, SelectSquares)] =
      sys.error("Can't selectSquares in fairysf")

    def diceRoll(
      dice: List[Int],
      metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, DiceRoll)] =
      sys.error("Can't diceroll in fairysf")

    def randomizeDiceRoll: Option[DiceRoll] = None

    def copy(clock: Option[ClockBase]): Game =
      FairySF(g.copy(clock = clock))

    def copy(plies: Int, turnCount: Int, startedAtPly: Int, startedAtTurn: Int): Game =
      FairySF(
        g.copy(
          plies = plies,
          turnCount = turnCount,
          startedAtPly = startedAtPly,
          startedAtTurn = startedAtTurn
        )
      )

    def copy(
        clock: Option[ClockBase],
        plies: Int,
        turnCount: Int,
        startedAtPly: Int,
        startedAtTurn: Int
    ): Game =
      FairySF(
        g.copy(
          clock = clock,
          plies = plies,
          turnCount = turnCount,
          startedAtPly = startedAtPly,
          startedAtTurn = startedAtTurn
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

    def hasJustSwitchedTurns: Boolean = g.hasJustSwitchedTurns

    def withTurnsAndPlies(p: Int, t: Int): Game = FairySF(g.withTurnsAndPlies(p, t))

    def toFairySF: fairysf.Game           = g
    def toChess: chess.Game               = sys.error("Can't turn a fairysf game into a chess game")
    def toDraughts: draughts.DraughtsGame = sys.error("Can't turn a fairysf game into a draughts game")
    def toSamurai: samurai.Game           = sys.error("Can't turn a fairysf game into a samurai game")
    def toTogyzkumalak: togyzkumalak.Game = sys.error("Can't turn a fairysf game into a togyzkumalak game")
    def toGo: go.Game                     = sys.error("Can't turn a fairysf game into a go game")
    def toBackgammon: backgammon.Game     = sys.error("Can't turn a fairysf game into a backgammon game")

  }

  final case class Samurai(g: samurai.Game)
      extends Game(
        Situation.Samurai(g.situation),
        g.actionStrs,
        g.clock,
        g.plies,
        g.turnCount,
        g.startedAtPly,
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
      case (Pos.Samurai(orig), Pos.Samurai(dest)) =>
        g.apply(orig, dest, None, metrics)
          .toEither
          .map(t => (Samurai(t._1), Move.Samurai(t._2)))
          .toValidated
      case _                                      => sys.error("Not passed Samurai objects")
    }

    def apply(action: Action): Game =
      action match {
        case (Move.Samurai(move)) => Samurai(g.apply(move))
        case _                    => sys.error("Not passed Samurai objects")
      }

    def drop(
        role: Role,
        pos: Pos,
        metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, Drop)] = sys.error("Can't drop in Samurai")

    def pass(metrics: MoveMetrics = MoveMetrics()): Validated[String, (Game, Pass)] =
      sys.error("Can't pass in Samurai")

    def selectSquares(
        squares: List[Pos],
        metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, SelectSquares)] =
      sys.error("Can't selectSquares in Samurai")

    def diceRoll(
      dice: List[Int],
      metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, DiceRoll)] =
      sys.error("Can't diceroll in samurai")

    def randomizeDiceRoll: Option[DiceRoll] = None

    def copy(clock: Option[ClockBase]): Game =
      Samurai(g.copy(clock = clock))

    def copy(plies: Int, turnCount: Int, startedAtPly: Int, startedAtTurn: Int): Game =
      Samurai(
        g.copy(
          plies = plies,
          turnCount = turnCount,
          startedAtPly = startedAtPly,
          startedAtTurn = startedAtTurn
        )
      )

    def copy(
        clock: Option[ClockBase],
        plies: Int,
        turnCount: Int,
        startedAtPly: Int,
        startedAtTurn: Int
    ): Game =
      Samurai(
        g.copy(
          clock = clock,
          plies = plies,
          turnCount = turnCount,
          startedAtPly = startedAtPly,
          startedAtTurn = startedAtTurn
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

    def hasJustSwitchedTurns: Boolean = g.hasJustSwitchedTurns

    def withTurnsAndPlies(p: Int, t: Int): Game = Samurai(g.withTurnsAndPlies(p, t))

    def toFairySF: fairysf.Game           = sys.error("Can't turn a samurai game into a fairysf game")
    def toChess: chess.Game               = sys.error("Can't turn a samurai game into a chess game")
    def toDraughts: draughts.DraughtsGame = sys.error("Can't turn a samurai game into a draughts game")
    def toSamurai: samurai.Game           = g
    def toTogyzkumalak: togyzkumalak.Game = sys.error("Can't turn a samurai game into a togyzkumalak game")
    def toGo: go.Game                     = sys.error("Can't turn a samurai game into a go game")
    def toBackgammon: backgammon.Game     = sys.error("Can't turn a samurai game into a backgammon game")

  }

  final case class Togyzkumalak(g: togyzkumalak.Game)
      extends Game(
        Situation.Togyzkumalak(g.situation),
        g.actionStrs,
        g.clock,
        g.plies,
        g.turnCount,
        g.startedAtPly,
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
      case (Pos.Togyzkumalak(orig), Pos.Togyzkumalak(dest)) =>
        g.apply(orig, dest, None, metrics)
          .toEither
          .map(t => (Togyzkumalak(t._1), Move.Togyzkumalak(t._2)))
          .toValidated
      case _                                                => sys.error("Not passed Togyzkumalak objects")
    }

    def apply(action: Action): Game =
      action match {
        case (Move.Togyzkumalak(move)) => Togyzkumalak(g.apply(move))
        case _                         => sys.error("Not passed Togyzkumalak objects")
      }

    def drop(
        role: Role,
        pos: Pos,
        metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, Drop)] = sys.error("Can't drop in Togyzkumalak")

    def pass(metrics: MoveMetrics = MoveMetrics()): Validated[String, (Game, Pass)] =
      sys.error("Can't pass in Togyzkumalak")

    def selectSquares(
        squares: List[Pos],
        metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, SelectSquares)] =
      sys.error("Can't selectSquares in Togyzkumalak")

    def diceRoll(
      dice: List[Int],
      metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, DiceRoll)] =
      sys.error("Can't diceroll in togyzkumalak")

    def randomizeDiceRoll: Option[DiceRoll] = None

    def copy(clock: Option[ClockBase]): Game =
      Togyzkumalak(g.copy(clock = clock))

    def copy(plies: Int, turnCount: Int, startedAtPly: Int, startedAtTurn: Int): Game =
      Togyzkumalak(
        g.copy(
          plies = plies,
          turnCount = turnCount,
          startedAtPly = startedAtPly,
          startedAtTurn = startedAtTurn
        )
      )

    def copy(
        clock: Option[ClockBase],
        plies: Int,
        turnCount: Int,
        startedAtPly: Int,
        startedAtTurn: Int
    ): Game =
      Togyzkumalak(
        g.copy(
          clock = clock,
          plies = plies,
          turnCount = turnCount,
          startedAtPly = startedAtPly,
          startedAtTurn = startedAtTurn
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

    def hasJustSwitchedTurns: Boolean = g.hasJustSwitchedTurns

    def withTurnsAndPlies(p: Int, t: Int): Game = Togyzkumalak(g.withTurnsAndPlies(p, t))

    def toFairySF: fairysf.Game           = sys.error("Can't turn a togyzkumalak game into a fairysf game")
    def toChess: chess.Game               = sys.error("Can't turn a togyzkumalak game into a chess game")
    def toDraughts: draughts.DraughtsGame = sys.error("Can't turn a togyzkumalak game into a draughts game")
    def toSamurai: samurai.Game           = sys.error("Can't turn a togyzkumalak game into a samurai game")
    def toTogyzkumalak: togyzkumalak.Game = g
    def toGo: go.Game                     = sys.error("Can't turn a togyzkumalak game into a go game")
    def toBackgammon: backgammon.Game     = sys.error("Can't turn a togyzkumalak game into a backgammon game")

  }

  final case class Go(g: go.Game)
      extends Game(
        Situation.Go(g.situation),
        g.actionStrs,
        g.clock,
        g.plies,
        g.turnCount,
        g.startedAtPly,
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
    ): Validated[String, (Game, Move)] = sys.error("Can't move in Go")

    def apply(action: Action): Game =
      action match {
        case (Pass.Go(pass)) => Go(g.applyPass(pass))
        case (Drop.Go(drop)) => Go(g.applyDrop(drop))
        case _               => sys.error("Not passed Go objects")
      }

    def drop(
        role: Role,
        pos: Pos,
        metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, Drop)] = (role, pos) match {
      case (Role.GoRole(role), Pos.Go(pos)) =>
        g.drop(role, pos, metrics)
          .toEither
          .map(t => (Go(t._1), Drop.Go(t._2)))
          .toValidated
      case _                                => sys.error("Not passed Go objects")
    }

    def pass(metrics: MoveMetrics = MoveMetrics()): Validated[String, (Game, Pass)] =
      g.pass(metrics).toEither.map(t => (Go(t._1), Pass.Go(t._2))).toValidated

    def selectSquares(
        squares: List[Pos],
        metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, SelectSquares)] =
      g.selectSquares(
        squares.map {
          case Pos.Go(pos) => pos
          case _           => sys.error("Not passed Go pos objects")
        },
        metrics
      ).toEither
        .map(t => (Go(t._1), SelectSquares.Go(t._2)))
        .toValidated

    def diceRoll(
      dice: List[Int],
      metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, DiceRoll)] =
      sys.error("Can't diceroll in Go")

    def randomizeDiceRoll: Option[DiceRoll] = None

    def copy(clock: Option[ClockBase]): Game = Go(g.copy(clock = clock))

    def copy(plies: Int, turnCount: Int, startedAtPly: Int, startedAtTurn: Int): Game =
      Go(
        g.copy(
          plies = plies,
          turnCount = turnCount,
          startedAtPly = startedAtPly,
          startedAtTurn = startedAtTurn
        )
      )

    def copy(
        clock: Option[ClockBase],
        plies: Int,
        turnCount: Int,
        startedAtPly: Int,
        startedAtTurn: Int
    ): Game =
      Go(
        g.copy(
          clock = clock,
          plies = plies,
          turnCount = turnCount,
          startedAtPly = startedAtPly,
          startedAtTurn = startedAtTurn
        )
      )

    def copy(situation: Situation, plies: Int, turnCount: Int): Game = situation match {
      case Situation.Go(situation) =>
        Go(g.copy(situation = situation, plies = plies, turnCount = turnCount))
      case _                       =>
        sys.error("Unable to copy go game with non-go arguments")
    }

    def copy(situation: Situation): Game = situation match {
      case Situation.Go(situation) => Go(g.copy(situation = situation))
      case _                       => sys.error("Unable to copy go game with non-go arguments")
    }

    def hasJustSwitchedTurns: Boolean = g.hasJustSwitchedTurns

    def withTurnsAndPlies(p: Int, t: Int): Game = Go(g.withTurnsAndPlies(p, t))

    def toFairySF: fairysf.Game           = sys.error("Can't turn a go game into a fairysf game")
    def toChess: chess.Game               = sys.error("Can't turn a go game into a chess game")
    def toDraughts: draughts.DraughtsGame = sys.error("Can't turn a go game into a draughts game")
    def toSamurai: samurai.Game           = sys.error("Can't turn a go game into a samurai game")
    def toTogyzkumalak: togyzkumalak.Game = sys.error("Can't turn a go game into a togyzkumalak game")
    def toGo: go.Game                     = g
    def toBackgammon: backgammon.Game     = sys.error("Can't turn a go game into a backgammon game")

  }

  final case class Backgammon(g: backgammon.Game)
      extends Game(
        Situation.Backgammon(g.situation),
        g.actionStrs,
        g.clock,
        g.plies,
        g.turnCount,
        g.startedAtPly,
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
      case (Pos.Backgammon(orig), Pos.Backgammon(dest)) =>
        g.apply(orig, dest, None, metrics)
          .toEither
          .map(t => (Backgammon(t._1), Move.Backgammon(t._2)))
          .toValidated
      case _                                            => sys.error("Not passed Backgammon objects")
    }

    def apply(action: Action): Game =
      action match {
        case (Move.Backgammon(move)) => Backgammon(g.apply(move))
        case _                       => sys.error("Not passed Backgammon objects")
      }

    def drop(
        role: Role,
        pos: Pos,
        metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, Drop)] = sys.error("Can't drop in Backgammon")

    def pass(metrics: MoveMetrics = MoveMetrics()): Validated[String, (Game, Pass)] =
      sys.error("Can't pass in Backgammon")

    def selectSquares(
        squares: List[Pos],
        metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, SelectSquares)] =
      sys.error("Can't selectSquares in Backgammon")

    def diceRoll(
      dice: List[Int],
      metrics: MoveMetrics = MoveMetrics()
    ): Validated[String, (Game, DiceRoll)] =
      g.diceRoll(dice, metrics)
        .toEither
        .map(t => (Backgammon(t._1), DiceRoll.Backgammon(t._2)))
        .toValidated

    def randomizeDiceRoll: Option[DiceRoll] = g.randomizeDiceRoll

    def copy(clock: Option[ClockBase]): Game =
      Backgammon(g.copy(clock = clock))

    def copy(plies: Int, turnCount: Int, startedAtPly: Int, startedAtTurn: Int): Game =
      Backgammon(
        g.copy(
          plies = plies,
          turnCount = turnCount,
          startedAtPly = startedAtPly,
          startedAtTurn = startedAtTurn
        )
      )

    def copy(
        clock: Option[ClockBase],
        plies: Int,
        turnCount: Int,
        startedAtPly: Int,
        startedAtTurn: Int
    ): Game =
      Backgammon(
        g.copy(
          clock = clock,
          plies = plies,
          turnCount = turnCount,
          startedAtPly = startedAtPly,
          startedAtTurn = startedAtTurn
        )
      )

    def copy(situation: Situation, plies: Int, turnCount: Int): Game = situation match {
      case Situation.Backgammon(situation) =>
        Backgammon(g.copy(situation = situation, plies = plies, turnCount = turnCount))
      case _                               =>
        sys.error("Unable to copy backgammon game with non-backgammon arguments")
    }
    def copy(situation: Situation): Game                             = situation match {
      case Situation.Backgammon(situation) => Backgammon(g.copy(situation = situation))
      case _                               => sys.error("Unable to copy backgammon game with non-backgammon arguments")
    }

    def hasJustSwitchedTurns: Boolean = g.hasJustSwitchedTurns

    def withTurnsAndPlies(p: Int, t: Int): Game = Backgammon(g.withTurnsAndPlies(p, t))

    def toFairySF: fairysf.Game           = sys.error("Can't turn a backgammon game into a fairysf game")
    def toChess: chess.Game               = sys.error("Can't turn a backgammon game into a chess game")
    def toDraughts: draughts.DraughtsGame = sys.error("Can't turn a backgammon game into a draughts game")
    def toSamurai: samurai.Game           = sys.error("Can't turn a backgammon game into a samurai game")
    def toTogyzkumalak: togyzkumalak.Game = sys.error("Can't turn a backgammon game into a togyzkumalak game")
    def toGo: go.Game                     = sys.error("Can't turn a backgammon game into a go game")
    def toBackgammon: backgammon.Game     = g

  }

  def apply(
      lib: GameLogic,
      situation: Situation,
      actionStrs: VActionStrs = Vector(),
      clock: Option[ClockBase] = None,
      plies: Int = 0,
      turnCount: Int = 0,
      startedAtPly: Int = 0,
      startedAtTurn: Int = 0
  ): Game = (lib, situation) match {
    case (GameLogic.Draughts(), Situation.Draughts(situation))         =>
      Draughts(
        draughts.DraughtsGame(situation, actionStrs, clock, plies, turnCount, startedAtPly, startedAtTurn)
      )
    case (GameLogic.Chess(), Situation.Chess(situation))               =>
      Chess(chess.Game(situation, actionStrs, clock, plies, turnCount, startedAtPly, startedAtTurn))
    case (GameLogic.FairySF(), Situation.FairySF(situation))           =>
      FairySF(fairysf.Game(situation, actionStrs, clock, plies, turnCount, startedAtPly, startedAtTurn))
    case (GameLogic.Samurai(), Situation.Samurai(situation))           =>
      Samurai(samurai.Game(situation, actionStrs, clock, plies, turnCount, startedAtPly, startedAtTurn))
    case (GameLogic.Togyzkumalak(), Situation.Togyzkumalak(situation)) =>
      Togyzkumalak(
        togyzkumalak.Game(situation, actionStrs, clock, plies, turnCount, startedAtPly, startedAtTurn)
      )
    case (GameLogic.Go(), Situation.Go(situation))                     =>
      Go(go.Game(situation, actionStrs, clock, plies, turnCount, startedAtPly, startedAtTurn))
    case (GameLogic.Backgammon(), Situation.Backgammon(situation))     =>
      Backgammon(backgammon.Game(situation, actionStrs, clock, plies, turnCount, startedAtPly, startedAtTurn))

    case _ => sys.error("Mismatched gamelogic types 32")
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
    case (GameLogic.Go(), Variant.Go(variant))                     =>
      Go(go.Game.apply(variant))
    case (GameLogic.Backgammon(), Variant.Backgammon(variant))     =>
      Backgammon(backgammon.Game.apply(variant))
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
    case GameLogic.Go()           =>
      Go(go.Game.apply(variant.map(_.toGo), fen.map(_.toGo)))
    case GameLogic.Backgammon()   =>
      Backgammon(backgammon.Game.apply(variant.map(_.toBackgammon), fen.map(_.toBackgammon)))
    case _                        => sys.error("Mismatched gamelogic types 36")
  }

  def wrap(g: chess.Game)            = Chess(g)
  def wrap(g: draughts.DraughtsGame) = Draughts(g)
  def wrap(g: fairysf.Game)          = FairySF(g)
  def wrap(g: samurai.Game)          = Samurai(g)
  def wrap(g: togyzkumalak.Game)     = Togyzkumalak(g)
  def wrap(g: go.Game)               = Go(g)
  def wrap(g: backgammon.Game)       = Backgammon(g)

}
