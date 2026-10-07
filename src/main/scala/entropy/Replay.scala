package strategygames.entropy

import cats.data.Validated
import cats.implicits._
import scalalib.extensions.*

import strategygames.entropy.format.pgn.Reader
import strategygames.entropy.format.{ FEN, Forsyth, Uci }
import strategygames.ActionStrs

case class Replay(setup: Game, actions: List[Action], state: Game) {

  lazy val chronoPlies = actions.reverse

  lazy val chronoActions: List[List[Action]] =
    chronoPlies
      .drop(1)
      .foldLeft(List(chronoPlies.take(1))) { case (turn, action) =>
        if (turn.head.head.player != action.player) List(action) +: turn
        else (turn.head :+ action) +: turn.tail
      }
      .reverse

  def addAction(action: Action) = action match {
    case m: Move         => copy(actions = m :: actions, state = state.apply(m))
    case d: Drop         => copy(actions = d :: actions, state = state.apply(d))
    case p: Pass         => copy(actions = p :: actions, state = state.apply(p))
    case dc: DrawCounter => copy(actions = dc :: actions, state = state.apply(dc))
    case a               => sys.error(s"Unexpected entropy action in replay: ${a}")
  }

}

object Replay {

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.entropy.variant.Variant
  ): Validated[String, Reader.Result] = {
    val fen                            = initialFen.getOrElse(variant.initialFen)
    val (init, gameWithActions, error) = gameWithActionWhileValid(actionStrs, fen, variant)
    val game                           = gameWithActions.lastOption.map(_._1).getOrElse(init)

    error match {
      case None      =>
        Validated.valid(
          Reader.Result.Complete(new Replay(init, gameWithActions.map(_._2).reverse, game))
        )
      case Some(msg) => Validated.invalid(msg)
    }
  }

  // the wrapper layer passes the players through; entropy derives whose turn it is from the
  // actions themselves, so they are accepted and ignored
  def apply(
      actionStrs: ActionStrs,
      @annotation.nowarn startPlayer: strategygames.Player,
      @annotation.nowarn activePlayer: strategygames.Player,
      initialFen: Option[FEN],
      variant: strategygames.entropy.variant.Variant
  ): Validated[String, Reader.Result] = apply(actionStrs, initialFen, variant)

  def makeGame(variant: strategygames.entropy.variant.Variant, initialFen: Option[FEN]): Game = {
    val g = Game(variant.some, initialFen)
    g.copy(startedAtPly = g.plies, startedAtTurn = g.turnCount)
  }

  private def gameWithActionWhileValid(
      actionStrs: ActionStrs,
      initialFen: FEN,
      variant: strategygames.entropy.variant.Variant
  ): (Game, List[(Game, Action)], Option[String]) = {
    val init = makeGame(variant, initialFen.some)

    val (_, played, error) =
      actionStrs.flatten.foldLeft((init, List.empty[(Game, Action)], Option.empty[String])) {
        case (done @ (_, _, Some(_)), _)        => done
        case ((state, played, None), actionStr) =>
          Uci(actionStr) match {
            case None      => (state, played, Some(s"Cannot read action: ${actionStr}"))
            case Some(uci) =>
              state.apply(uci) match {
                case Validated.Valid((next, action)) => (next, (next, action) :: played, None)
                case Validated.Invalid(err)          => (state, played, Some(err))
              }
          }
      }

    (init, played.reverse, error)
  }

  def gameWithUciWhileValid(
      actionStrs: ActionStrs,
      initialFen: FEN,
      variant: strategygames.entropy.variant.Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[String]) = {
    val (game, gameWithActions, error) = gameWithActionWhileValid(actionStrs, initialFen, variant)
    (
      game,
      gameWithActions.map { case (state, action) => (state, Uci.WithSan(action.toUci, "NOSAN")) },
      error
    )
  }

  private def initialFenToSituation(
      initialFen: Option[FEN],
      variant: strategygames.entropy.variant.Variant
  ): Situation =
    initialFen.flatMap(Forsyth.<<) | Situation(variant)

  private def recursiveSituationsFromUci(
      sit: Situation,
      ucis: List[Uci]
  ): Validated[String, List[Situation]] =
    ucis match {
      case Nil         => Validated.valid(Nil)
      case uci :: rest =>
        uci(sit) andThen { action =>
          val after = action.situationAfter
          recursiveSituationsFromUci(after, rest) map { after :: _ }
        }
    }

  def situationsFromUci(
      ucis: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.entropy.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    recursiveSituationsFromUci(sit, ucis) map { sit :: _ }
  }

  def boardsFromUci(
      ucis: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.entropy.variant.Variant
  ): Validated[String, List[Board]] = situationsFromUci(ucis, initialFen, variant) map (_ map (_.board))

  private def recursiveGamesFromUci(
      game: Game,
      ucis: List[Uci]
  ): Validated[String, List[Game]] =
    ucis match {
      case Nil         => Validated.valid(List(game))
      case uci :: rest =>
        game.apply(uci) andThen { case (game, _) =>
          recursiveGamesFromUci(game, rest) map { game :: _ }
        }
    }

  def gameFromUciStrings(
      uciStrings: List[String],
      initialFen: Option[FEN],
      variant: strategygames.entropy.variant.Variant
  ): Validated[String, Game] = {
    val init = makeGame(variant, initialFen)
    val ucis = uciStrings.flatMap(Uci.apply(_))
    if (uciStrings.size != ucis.size) Validated.invalid("Invalid Ucis")
    else recursiveGamesFromUci(init, ucis).map(_.last)
  }

  private def recursiveReplayFromUci(replay: Replay, ucis: List[Uci]): Validated[String, Replay] =
    ucis match {
      case Nil         => Validated.valid(replay)
      case uci :: rest =>
        uci(replay.state.situation) andThen { action =>
          recursiveReplayFromUci(replay addAction action, rest)
        }
    }

  def apply(
      ucis: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.entropy.variant.Variant
  ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), ucis)

  def situations(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.entropy.variant.Variant
  ): Validated[String, List[Situation]] =
    apply(actionStrs, initialFen, variant) andThen (_.valid) map { replay =>
      replay.setup.situation :: replay.chronoPlies.map(_.situationAfter)
    }

  def boards(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.entropy.variant.Variant
  ): Validated[String, List[Board]] =
    situations(actionStrs, initialFen, variant) map (_ map (_.board))

  def plyAtFen(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.entropy.variant.Variant,
      atFen: FEN
  ): Validated[String, Int] =
    situations(actionStrs, initialFen, variant) map { sits =>
      sits.indexWhere(s => Forsyth.exportBoard(s.board) == atFen.value) max 0
    }

}
