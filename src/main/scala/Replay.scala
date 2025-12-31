package strategygames

import cats.data.Validated
import cats.implicits._

import variant.Variant
import format.{ FEN, Uci }

sealed abstract class Replay(val setup: Game, val actions: List[Action], val state: Game) {

  lazy val chronoPlies = actions.reverse

  // https://stackoverflow.com/questions/53016370/how-to-convert-a-list-to-list-of-lists-by-group-the-elements-when-an-element-rep
  lazy val chronoActions: List[List[Action]] =
    chronoPlies
      .drop(1)
      .foldLeft(List(chronoPlies.take(1))) { case (turn, action) =>
        if (turn.head.head.player != action.player) {
          List(action) +: turn
        } else {
          (turn.head :+ action) +: turn.tail
        }
      }
      .reverse

  // this is only used by fishnet which wants to deal in single ply per turn states
  def actionAtPly(ply: Int): Option[Action] =
    chronoPlies lift (ply - 1 - setup.startedAtPly)

  // TODO: If we had a case class this would be automatic.
  def copy(state: Game): Replay

}

//lots of methods not wrapped, due to differences of Traversable/Iterable, and FEN/String
object Replay {

  final case class Chess(r: chess.Replay)
      extends Replay(
        Game.Chess(r.setup),
        r.actions.map(m =>
          m match {
            case m: chess.Move => Move.Chess(m)
            case d: chess.Drop => Drop.Chess(d)
          }
        ),
        Game.Chess(r.state)
      ) {
    def copy(state: Game): Replay = state match {
      case Game.Chess(state) => Replay.wrap(r.copy(state = state))
      case _                 => sys.error("Unable to copy a chess replay with a non-chess state")
    }
  }

  final case class Draughts(r: draughts.Replay)
      extends Replay(
        Game.Draughts(r.setup),
        r.actions.map((m: draughts.Move) => Move.Draughts(m)),
        Game.Draughts(r.state)
      ) {
    def copy(state: Game): Replay = state match {
      case Game.Draughts(state) => Replay.wrap(r.copy(state = state))
      case _                    => sys.error("Unable to copy a draughts replay with a non-draughts state")
    }
  }

  final case class FairySF(r: fairysf.Replay)
      extends Replay(
        Game.FairySF(r.setup),
        r.actions.map(m =>
          m match {
            case m: fairysf.Move => Move.FairySF(m)
            case d: fairysf.Drop => Drop.FairySF(d)
          }
        ),
        Game.FairySF(r.state)
      ) {
    def copy(state: Game): Replay = state match {
      case Game.FairySF(state) => Replay.wrap(r.copy(state = state))
      case _                   => sys.error("Unable to copy a fairysf replay with a non-fairysf state")
    }
  }

  final case class Samurai(r: samurai.Replay)
      extends Replay(
        Game.Samurai(r.setup),
        r.actions.map((m: samurai.Move) => Move.Samurai(m)),
        Game.Samurai(r.state)
      ) {
    def copy(state: Game): Replay = state match {
      case Game.Samurai(state) => Replay.wrap(r.copy(state = state))
      case _                   => sys.error("Unable to copy a samurai replay with a non-samurai state")
    }
  }

  final case class Togyzkumalak(r: togyzkumalak.Replay)
      extends Replay(
        Game.Togyzkumalak(r.setup),
        r.actions.map((m: togyzkumalak.Move) => Move.Togyzkumalak(m)),
        Game.Togyzkumalak(r.state)
      ) {
    def copy(state: Game): Replay = state match {
      case Game.Togyzkumalak(state) => Replay.wrap(r.copy(state = state))
      case _                        => sys.error("Unable to copy a togyzkumalak replay with a non-togyzkumalak state")
    }
  }

  final case class Go(r: go.Replay)
      extends Replay(
        Game.Go(r.setup),
        r.actions.map {
          case d: go.Drop           => Drop.Go(d)
          case p: go.Pass           => Pass.Go(p)
          case ss: go.SelectSquares => SelectSquares.Go(ss)
        },
        Game.Go(r.state)
      ) {
    def copy(state: Game): Replay = state match {
      case Game.Go(state) => Replay.wrap(r.copy(state = state))
      case _              => sys.error("Unable to copy a go replay with a non-go state")
    }
  }

  final case class Backgammon(r: backgammon.Replay)
      extends Replay(
        Game.Backgammon(r.setup),
        r.actions.map {
          case m: backgammon.Move        => Move.Backgammon(m)
          case d: backgammon.Drop        => Drop.Backgammon(d)
          case l: backgammon.Lift        => Lift.Backgammon(l)
          case dr: backgammon.DiceRoll   => DiceRoll.Backgammon(dr)
          case ca: backgammon.CubeAction => CubeAction.Backgammon(ca)
          case et: backgammon.EndTurn    => EndTurn.Backgammon(et)
        },
        Game.Backgammon(r.state)
      ) {
    def copy(state: Game): Replay = state match {
      case Game.Backgammon(state) => Replay.wrap(r.copy(state = state))
      case _                      => sys.error("Unable to copy a backgammon replay with a non-backgammon state")
    }
  }

  final case class Abalone(r: abalone.Replay)
      extends Replay(
        Game.Abalone(r.setup),
        r.actions.map((m: abalone.Move) => Move.Abalone(m)),
        Game.Abalone(r.state)
      ) {
    def copy(state: Game): Replay = state match {
      case Game.Abalone(state) => Replay.wrap(r.copy(state = state))
      case _                   => sys.error("Unable to copy a abalone replay with a non-abalone state")
    }
  }

  final case class Dameo(r: dameo.Replay)
      extends Replay(
        Game.Dameo(r.setup),
        r.actions.map((m: dameo.Move) => Move.Dameo(m)),
        Game.Dameo(r.state)
      ) {
    def copy(state: Game): Replay = state match {
      case Game.Dameo(state) => Replay.wrap(r.copy(state = state))
      case _                 => sys.error("Unable to copy a dameo replay with a non-dameo state")
    }
  }

  def apply(lib: GameLogic, setup: Game, actions: List[Action], state: Game): Replay =
    (lib, setup, state) match {
      case (GameLogic.Draughts(), Game.Draughts(setup), Game.Draughts(state))             =>
        Draughts(draughts.Replay(setup, actions.map(Action.toDraughts), state))
      case (GameLogic.Chess(), Game.Chess(setup), Game.Chess(state))                      =>
        Chess(chess.Replay(setup, actions.map(Action.toChess), state))
      case (GameLogic.FairySF(), Game.FairySF(setup), Game.FairySF(state))                =>
        FairySF(fairysf.Replay(setup, actions.map(Action.toFairySF), state))
      case (GameLogic.Samurai(), Game.Samurai(setup), Game.Samurai(state))                =>
        Samurai(samurai.Replay(setup, actions.map(Action.toSamurai), state))
      case (GameLogic.Togyzkumalak(), Game.Togyzkumalak(setup), Game.Togyzkumalak(state)) =>
        Togyzkumalak(togyzkumalak.Replay(setup, actions.map(Action.toTogyzkumalak), state))
      case (GameLogic.Go(), Game.Go(setup), Game.Go(state))                               =>
        Go(go.Replay(setup, actions.map(Action.toGo), state))
      case (GameLogic.Backgammon(), Game.Backgammon(setup), Game.Backgammon(state))       =>
        Backgammon(backgammon.Replay(setup, actions.map(Action.toBackgammon), state))
      case (GameLogic.Abalone(), Game.Abalone(setup), Game.Abalone(state))                =>
        Abalone(abalone.Replay(setup, actions.map(Action.toAbalone), state))
      case (GameLogic.Dameo(), Game.Dameo(setup), Game.Dameo(state))                      =>
        Dameo(dameo.Replay(setup, actions.map(Action.toDameo), state))
      case _                                                                              => sys.error("Mismatched gamelogic types 5")
    }

  def gameWithUciWhileValid(
      lib: GameLogic,
      actionStrs: ActionStrs,
      startPlayer: Player,
      activePlayer: Player,
      initialFen: FEN,
      variant: Variant,
      iteratedCapts: Boolean = false
  ): (Game, List[(Game, Uci.WithSan)], Option[String]) = (lib, initialFen, variant) match {
    case (GameLogic.Draughts(), FEN.Draughts(initialFen), Variant.Draughts(variant))             =>
      draughts.Replay.gameWithUciWhileValid(
        actionStrs,
        initialFen,
        variant,
        iteratedCapts
      ) match {
        case (game, gameswithsan, message) =>
          (
            Game.Draughts(game),
            gameswithsan.map { case (g, u) => (Game.Draughts(g), Uci.DraughtsWithSan(u)) },
            message
          )
      }
    case (GameLogic.Chess(), FEN.Chess(initialFen), Variant.Chess(variant))                      =>
      chess.Replay.gameWithUciWhileValid(
        actionStrs,
        initialFen,
        variant
      ) match {
        case (game, gameswithsan, message) =>
          (
            Game.Chess(game),
            gameswithsan.map { case (g, u) => (Game.Chess(g), Uci.ChessWithSan(u)) },
            message
          )
      }
    case (GameLogic.FairySF(), FEN.FairySF(initialFen), Variant.FairySF(variant))                =>
      fairysf.Replay.gameWithUciWhileValid(
        actionStrs,
        initialFen,
        variant
      ) match {
        case (game, gameswithsan, message) =>
          (
            Game.FairySF(game),
            gameswithsan.map { case (g, u) => (Game.FairySF(g), Uci.FairySFWithSan(u)) },
            message
          )
      }
    case (GameLogic.Samurai(), FEN.Samurai(initialFen), Variant.Samurai(variant))                =>
      samurai.Replay.gameWithUciWhileValid(
        actionStrs,
        startPlayer,
        activePlayer,
        initialFen,
        variant
      ) match {
        case (game, gameswithsan, message) =>
          (
            Game.Samurai(game),
            gameswithsan.map { case (g, u) => (Game.Samurai(g), Uci.SamuraiWithSan(u)) },
            message
          )
      }
    case (GameLogic.Togyzkumalak(), FEN.Togyzkumalak(initialFen), Variant.Togyzkumalak(variant)) =>
      togyzkumalak.Replay.gameWithUciWhileValid(
        actionStrs,
        startPlayer,
        activePlayer,
        initialFen,
        variant
      ) match {
        case (game, gameswithsan, message) =>
          (
            Game.Togyzkumalak(game),
            gameswithsan.map { case (g, u) => (Game.Togyzkumalak(g), Uci.TogyzkumalakWithSan(u)) },
            message
          )
      }
    case (GameLogic.Go(), FEN.Go(initialFen), Variant.Go(variant))                               =>
      go.Replay.gameWithUciWhileValid(
        actionStrs,
        startPlayer,
        activePlayer,
        initialFen,
        variant
      ) match {
        case (game, gameswithsan, message) =>
          (
            Game.Go(game),
            gameswithsan.map { case (g, u) => (Game.Go(g), Uci.GoWithSan(u)) },
            message
          )
      }
    case (GameLogic.Backgammon(), FEN.Backgammon(initialFen), Variant.Backgammon(variant))       =>
      backgammon.Replay.gameWithUciWhileValid(
        actionStrs,
        initialFen,
        variant
      ) match {
        case (game, gameswithsan, message) =>
          (
            Game.Backgammon(game),
            gameswithsan.map { case (g, u) => (Game.Backgammon(g), Uci.BackgammonWithSan(u)) },
            message
          )
      }
    case (GameLogic.Abalone(), FEN.Abalone(initialFen), Variant.Abalone(variant))                =>
      abalone.Replay.gameWithUciWhileValid(
        actionStrs,
        startPlayer,
        activePlayer,
        initialFen,
        variant
      ) match {
        case (game, gameswithsan, message) =>
          (
            Game.Abalone(game),
            gameswithsan.map { case (g, u) => (Game.Abalone(g), Uci.AbaloneWithSan(u)) },
            message
          )
      }
    case (GameLogic.Dameo(), FEN.Dameo(initialFen), Variant.Dameo(variant))                      =>
      dameo.Replay.gameWithUciWhileValid(
        actionStrs,
        initialFen,
        variant
      ) match {
        case (game, gameswithsan, message) =>
          (
            Game.Dameo(game),
            gameswithsan.map { case (g, u) => (Game.Dameo(g), Uci.DameoWithSan(u)) },
            message
          )
      }
    case _                                                                                       => sys.error("Mismatched gamelogic types 7")
  }

  def boards(
      lib: GameLogic,
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, List[Board]] =
    situations(lib, actionStrs, initialFen, variant, finalSquare) map (_ map (_.board))

  def situations(
      lib: GameLogic,
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, List[Situation]] = (lib, variant) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant))         =>
      draughts.Replay
        .situations(actionStrs, initialFen.map(_.toDraughts), variant, finalSquare)
        .toEither
        .map(s => s.map(Situation.Draughts.apply))
        .toValidated
    case (GameLogic.Chess(), Variant.Chess(variant))               =>
      chess.Replay
        .situations(actionStrs, initialFen.map(_.toChess), variant)
        .toEither
        .map(s => s.map(Situation.Chess.apply))
        .toValidated
    case (GameLogic.FairySF(), Variant.FairySF(variant))           =>
      fairysf.Replay
        .situations(actionStrs, initialFen.map(_.toFairySF), variant)
        .toEither
        .map(s => s.map(Situation.FairySF.apply))
        .toValidated
    case (GameLogic.Samurai(), Variant.Samurai(variant))           =>
      samurai.Replay
        .situations(actionStrs, initialFen.map(_.toSamurai), variant)
        .toEither
        .map(s => s.map(Situation.Samurai.apply))
        .toValidated
    case (GameLogic.Togyzkumalak(), Variant.Togyzkumalak(variant)) =>
      togyzkumalak.Replay
        .situations(actionStrs, initialFen.map(_.toTogyzkumalak), variant)
        .toEither
        .map(s => s.map(Situation.Togyzkumalak.apply))
        .toValidated
    case (GameLogic.Go(), Variant.Go(variant))                     =>
      go.Replay
        .situations(actionStrs, initialFen.map(_.toGo), variant)
        .toEither
        .map(s => s.map(Situation.Go.apply))
        .toValidated
    case (GameLogic.Backgammon(), Variant.Backgammon(variant))     =>
      backgammon.Replay
        .situations(actionStrs, initialFen.map(_.toBackgammon), variant)
        .toEither
        .map(s => s.map(Situation.Backgammon.apply))
        .toValidated
    case (GameLogic.Abalone(), Variant.Abalone(variant))           =>
      abalone.Replay
        .situations(actionStrs, initialFen.map(_.toAbalone), variant)
        .toEither
        .map(s => s.map(Situation.Abalone.apply))
        .toValidated
    case (GameLogic.Dameo(), Variant.Dameo(variant))               =>
      dameo.Replay
        .situations(actionStrs, initialFen.map(_.toDameo), variant)
        .toEither
        .map(s => s.map(Situation.Dameo.apply))
        .toValidated
    case _                                                         => sys.error("Mismatched gamelogic types 8")
  }

  private def draughtsUcis(ucis: List[Uci]): List[draughts.format.Uci] =
    ucis.flatMap(u =>
      u match {
        case u: Uci.Draughts => Some(u.unwrap)
        case _               => None
      }
    )

  private def chessUcis(ucis: List[Uci]): List[chess.format.Uci] =
    ucis.flatMap(u =>
      u match {
        case u: Uci.Chess => Some(u.unwrap)
        case _            => None
      }
    )

  private def fairysfUcis(ucis: List[Uci]): List[fairysf.format.Uci] =
    ucis.flatMap(u =>
      u match {
        case u: Uci.FairySF => Some(u.unwrap)
        case _              => None
      }
    )

  private def samuraiUcis(ucis: List[Uci]): List[samurai.format.Uci] =
    ucis.flatMap(u =>
      u match {
        case u: Uci.Samurai => Some(u.unwrap)
        case _              => None
      }
    )

  private def togyzkumalakUcis(ucis: List[Uci]): List[togyzkumalak.format.Uci] =
    ucis.flatMap(u =>
      u match {
        case u: Uci.Togyzkumalak => Some(u.unwrap)
        case _                   => None
      }
    )

  private def goUcis(ucis: List[Uci]): List[go.format.Uci] =
    ucis.flatMap(u =>
      u match {
        case u: Uci.Go => Some(u.unwrap)
        case _         => None
      }
    )

  private def backgammonUcis(ucis: List[Uci]): List[backgammon.format.Uci] =
    ucis.flatMap(u =>
      u match {
        case u: Uci.Backgammon => Some(u.unwrap)
        case _                 => None
      }
    )

  private def abaloneUcis(ucis: List[Uci]): List[abalone.format.Uci] =
    ucis.flatMap(u =>
      u match {
        case u: Uci.Abalone => Some(u.unwrap)
        case _              => None
      }
    )

  private def dameoUcis(ucis: List[Uci]): List[dameo.format.Uci] =
    ucis.flatMap(u =>
      u match {
        case u: Uci.Dameo => Some(u.unwrap)
        case _            => None
      }
    )

  def boardsFromUci(
      lib: GameLogic,
      ucis: List[Uci],
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, List[Board]] = (lib, variant) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant))         =>
      draughts.Replay
        .boardsFromUci(
          draughtsUcis(ucis),
          initialFen.map(_.toDraughts),
          variant,
          finalSquare
        )
        .toEither
        .map(b => b.map(Board.Draughts.apply))
        .toValidated
    case (GameLogic.Chess(), Variant.Chess(variant))               =>
      chess.Replay
        .boardsFromUci(chessUcis(ucis), initialFen.map(_.toChess), variant)
        .toEither
        .map(b => b.map(Board.Chess.apply))
        .toValidated
    case (GameLogic.FairySF(), Variant.FairySF(variant))           =>
      fairysf.Replay
        .boardsFromUci(fairysfUcis(ucis), initialFen.map(_.toFairySF), variant)
        .toEither
        .map(b => b.map(Board.FairySF.apply))
        .toValidated
    case (GameLogic.Samurai(), Variant.Samurai(variant))           =>
      samurai.Replay
        .boardsFromUci(samuraiUcis(ucis), initialFen.map(_.toSamurai), variant)
        .toEither
        .map(b => b.map(Board.Samurai.apply))
        .toValidated
    case (GameLogic.Togyzkumalak(), Variant.Togyzkumalak(variant)) =>
      togyzkumalak.Replay
        .boardsFromUci(togyzkumalakUcis(ucis), initialFen.map(_.toTogyzkumalak), variant)
        .toEither
        .map(b => b.map(Board.Togyzkumalak.apply))
        .toValidated
    case (GameLogic.Go(), Variant.Go(variant))                     =>
      go.Replay
        .boardsFromUci(goUcis(ucis), initialFen.map(_.toGo), variant)
        .toEither
        .map(b => b.map(Board.Go.apply))
        .toValidated
    case (GameLogic.Backgammon(), Variant.Backgammon(variant))     =>
      backgammon.Replay
        .boardsFromUci(backgammonUcis(ucis), initialFen.map(_.toBackgammon), variant)
        .toEither
        .map(b => b.map(Board.Backgammon.apply))
        .toValidated
    case (GameLogic.Abalone(), Variant.Abalone(variant))           =>
      abalone.Replay
        .boardsFromUci(abaloneUcis(ucis), initialFen.map(_.toAbalone), variant)
        .toEither
        .map(b => b.map(Board.Abalone.apply))
        .toValidated
    case (GameLogic.Dameo(), Variant.Dameo(variant))               =>
      dameo.Replay
        .boardsFromUci(dameoUcis(ucis), initialFen.map(_.toDameo), variant)
        .toEither
        .map(b => b.map(Board.Dameo.apply))
        .toValidated
    case _                                                         => sys.error("Mismatched gamelogic types 8a")
  }

  def situationsFromUci(
      lib: GameLogic,
      ucis: List[Uci],
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, List[Situation]] = (lib, variant) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant))         =>
      draughts.Replay
        .situationsFromUci(draughtsUcis(ucis), initialFen.map(_.toDraughts), variant, finalSquare)
        .toEither
        .map(s => s.map(Situation.Draughts.apply))
        .toValidated
    case (GameLogic.Chess(), Variant.Chess(variant))               =>
      chess.Replay
        .situationsFromUci(chessUcis(ucis), initialFen.map(_.toChess), variant)
        .toEither
        .map(s => s.map(Situation.Chess.apply))
        .toValidated
    case (GameLogic.FairySF(), Variant.FairySF(variant))           =>
      fairysf.Replay
        .situationsFromUci(fairysfUcis(ucis), initialFen.map(_.toFairySF), variant)
        .toEither
        .map(s => s.map(Situation.FairySF.apply))
        .toValidated
    case (GameLogic.Samurai(), Variant.Samurai(variant))           =>
      samurai.Replay
        .situationsFromUci(samuraiUcis(ucis), initialFen.map(_.toSamurai), variant)
        .toEither
        .map(s => s.map(Situation.Samurai.apply))
        .toValidated
    case (GameLogic.Togyzkumalak(), Variant.Togyzkumalak(variant)) =>
      togyzkumalak.Replay
        .situationsFromUci(togyzkumalakUcis(ucis), initialFen.map(_.toTogyzkumalak), variant)
        .toEither
        .map(s => s.map(Situation.Togyzkumalak.apply))
        .toValidated
    case (GameLogic.Go(), Variant.Go(variant))                     =>
      go.Replay
        .situationsFromUci(goUcis(ucis), initialFen.map(_.toGo), variant)
        .toEither
        .map(s => s.map(Situation.Go.apply))
        .toValidated
    case (GameLogic.Backgammon(), Variant.Backgammon(variant))     =>
      backgammon.Replay
        .situationsFromUci(backgammonUcis(ucis), initialFen.map(_.toBackgammon), variant)
        .toEither
        .map(s => s.map(Situation.Backgammon.apply))
        .toValidated
    case (GameLogic.Abalone(), Variant.Abalone(variant))           =>
      abalone.Replay
        .situationsFromUci(abaloneUcis(ucis), initialFen.map(_.toAbalone), variant)
        .toEither
        .map(s => s.map(Situation.Abalone.apply))
        .toValidated
    case (GameLogic.Dameo(), Variant.Dameo(variant))               =>
      dameo.Replay
        .situationsFromUci(dameoUcis(ucis), initialFen.map(_.toDameo), variant)
        .toEither
        .map(s => s.map(Situation.Dameo.apply))
        .toValidated
    case _                                                         => sys.error("Mismatched gamelogic types 9")
  }

  def gameFromUciStrings(
      lib: GameLogic,
      ucis: ActionStrs,
      activePlayer: Player,
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, Game] = (lib, variant) match {
    // All apart from Go can convert ucis to List[String] because the way the internal
    // function handles this List[String] is safe for multiaction
    case (GameLogic.Draughts(), Variant.Draughts(variant))         =>
      draughts.Replay
        .gameFromUciStrings(ucis.flatten.toList, initialFen.map(_.toDraughts), variant, finalSquare)
        .map(Game.Draughts.apply)
    case (GameLogic.Chess(), Variant.Chess(variant))               =>
      chess.Replay
        .gameFromUciStrings(ucis.flatten.toList, initialFen.map(_.toChess), variant)
        .map(Game.Chess.apply)
    case (GameLogic.FairySF(), Variant.FairySF(variant))           =>
      fairysf.Replay
        .gameFromUciStrings(ucis.flatten.toList, initialFen.map(_.toFairySF), variant)
        .map(Game.FairySF.apply)
    case (GameLogic.Samurai(), Variant.Samurai(variant))           =>
      samurai.Replay
        .gameFromUciStrings(ucis.flatten.toList, initialFen.map(_.toSamurai), variant)
        .map(Game.Samurai.apply)
    case (GameLogic.Togyzkumalak(), Variant.Togyzkumalak(variant)) =>
      togyzkumalak.Replay
        .gameFromUciStrings(ucis.flatten.toList, initialFen.map(_.toTogyzkumalak), variant)
        .map(Game.Togyzkumalak.apply)
    // Go doesnt convert ucis as it runs a different internal function that wants ActionStrs
    // due to optimisation issues
    case (GameLogic.Go(), Variant.Go(variant))                     =>
      go.Replay
        .gameFromUciStrings(ucis, activePlayer, initialFen.map(_.toGo), variant)
        .map(Game.Go.apply)
    case (GameLogic.Backgammon(), Variant.Backgammon(variant))     =>
      backgammon.Replay
        .gameFromUciStrings(ucis.flatten.toList, initialFen.map(_.toBackgammon), variant)
        .map(Game.Backgammon.apply)
    case (GameLogic.Abalone(), Variant.Abalone(variant))           =>
      abalone.Replay
        .gameFromUciStrings(ucis.flatten.toList, initialFen.map(_.toAbalone), variant)
        .map(Game.Abalone.apply)
    case (GameLogic.Dameo(), Variant.Dameo(variant))               =>
      dameo.Replay
        .gameFromUciStrings(ucis.flatten.toList, initialFen.map(_.toDameo), variant)
        .map(Game.Dameo.apply)
    case _                                                         => sys.error("Mismatched gamelogic types for Replay 10")
  }

  def apply(
      lib: GameLogic,
      ucis: List[Uci],
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, Replay] = (lib, variant) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant))         =>
      draughts
        .Replay(draughtsUcis(ucis), initialFen.map(_.toDraughts), variant, finalSquare)
        .toEither
        .map(r => Replay.Draughts(r))
        .toValidated
    case (GameLogic.Chess(), Variant.Chess(variant))               =>
      chess
        .Replay(chessUcis(ucis), initialFen.map(_.toChess), variant)
        .toEither
        .map(r => Replay.Chess(r))
        .toValidated
    case (GameLogic.FairySF(), Variant.FairySF(variant))           =>
      fairysf
        .Replay(fairysfUcis(ucis), initialFen.map(_.toFairySF), variant)
        .toEither
        .map(r => Replay.FairySF(r))
        .toValidated
    case (GameLogic.Samurai(), Variant.Samurai(variant))           =>
      samurai.Replay
        .apply(samuraiUcis(ucis), initialFen.map(_.toSamurai), variant)
        .toEither
        .map(r => Replay.Samurai(r))
        .toValidated
    case (GameLogic.Togyzkumalak(), Variant.Togyzkumalak(variant)) =>
      togyzkumalak.Replay
        .apply(togyzkumalakUcis(ucis), initialFen.map(_.toTogyzkumalak), variant)
        .toEither
        .map(r => Replay.Togyzkumalak(r))
        .toValidated
    case (GameLogic.Go(), Variant.Go(variant))                     =>
      go.Replay
        .apply(goUcis(ucis), initialFen.map(_.toGo), variant)
        .toEither
        .map(r => Replay.Go(r))
        .toValidated
    case (GameLogic.Backgammon(), Variant.Backgammon(variant))     =>
      backgammon.Replay
        .apply(backgammonUcis(ucis), initialFen.map(_.toBackgammon), variant)
        .toEither
        .map(r => Replay.Backgammon(r))
        .toValidated
    case (GameLogic.Abalone(), Variant.Abalone(variant))           =>
      abalone.Replay
        .apply(abaloneUcis(ucis), initialFen.map(_.toAbalone), variant)
        .toEither
        .map(r => Replay.Abalone(r))
        .toValidated
    case (GameLogic.Dameo(), Variant.Dameo(variant))               =>
      dameo.Replay
        .apply(dameoUcis(ucis), initialFen.map(_.toDameo), variant)
        .toEither
        .map(r => Replay.Dameo(r))
        .toValidated
    case _                                                         => sys.error("Mismatched gamelogic types Replay 11")
  }

  def plyAtFen(
      lib: GameLogic,
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: Variant,
      atFen: FEN
  ): Validated[String, Int] = (lib, variant, atFen) match {
    case (GameLogic.Draughts(), Variant.Draughts(variant), FEN.Draughts(atFen))             =>
      draughts.Replay.plyAtFen(actionStrs, initialFen.map(_.toDraughts), variant, atFen)
    case (GameLogic.Chess(), Variant.Chess(variant), FEN.Chess(atFen))                      =>
      chess.Replay.plyAtFen(actionStrs, initialFen.map(_.toChess), variant, atFen)
    case (GameLogic.FairySF(), Variant.FairySF(variant), FEN.FairySF(atFen))                =>
      fairysf.Replay.plyAtFen(actionStrs, initialFen.map(_.toFairySF), variant, atFen)
    case (GameLogic.Samurai(), Variant.Samurai(variant), FEN.Samurai(atFen))                =>
      samurai.Replay.plyAtFen(actionStrs, initialFen.map(_.toSamurai), variant, atFen)
    case (GameLogic.Togyzkumalak(), Variant.Togyzkumalak(variant), FEN.Togyzkumalak(atFen)) =>
      togyzkumalak.Replay.plyAtFen(actionStrs, initialFen.map(_.toTogyzkumalak), variant, atFen)
    case (GameLogic.Go(), Variant.Go(variant), FEN.Go(atFen))                               =>
      go.Replay.plyAtFen(actionStrs, initialFen.map(_.toGo), variant, atFen)
    case (GameLogic.Backgammon(), Variant.Backgammon(variant), FEN.Backgammon(atFen))       =>
      backgammon.Replay.plyAtFen(actionStrs, initialFen.map(_.toBackgammon), variant, atFen)
    case (GameLogic.Abalone(), Variant.Abalone(variant), FEN.Abalone(atFen))                =>
      abalone.Replay.plyAtFen(actionStrs, initialFen.map(_.toAbalone), variant, atFen)
    case (GameLogic.Dameo(), Variant.Dameo(variant), FEN.Dameo(atFen))                      =>
      dameo.Replay.plyAtFen(actionStrs, initialFen.map(_.toDameo), variant, atFen)
    case _                                                                                  => sys.error("Mismatched gamelogic types 10")
  }

  def wrap(r: chess.Replay)        = Chess(r)
  def wrap(r: draughts.Replay)     = Draughts(r)
  def wrap(r: fairysf.Replay)      = FairySF(r)
  def wrap(r: samurai.Replay)      = Samurai(r)
  def wrap(r: togyzkumalak.Replay) = Togyzkumalak(r)
  def wrap(r: go.Replay)           = Go(r)
  def wrap(r: backgammon.Replay)   = Backgammon(r)
  def wrap(r: abalone.Replay)      = Abalone(r)
  def wrap(r: dameo.Replay)        = Dameo(r)

}
