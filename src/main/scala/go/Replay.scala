package strategygames.go

import cats.data.Validated
import cats.data.Validated.valid
import cats.implicits._
import scalalib.extensions.*

import strategygames.Player
import strategygames.format.pgn.San
import strategygames.go.format.pgn.{ Parser, Reader }
import strategygames.go.format.{ FEN, Forsyth, Uci }
import strategygames.{
  Action => StratAction,
  ActionStrs,
  Drop => StratDrop,
  Pass => StratPass,
  SelectSquares => StratSelectSquares,
  Situation => StratSituation
}

case class Replay(setup: Game, actions: List[Action], state: Game) {

  lazy val chronoPlies = actions.reverse

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

  def addAction(action: Action) = action match {
    case d: Drop           =>
      copy(
        actions = d.applyVariantEffect :: actions,
        state = state.applyDrop(d)
      )
    case p: Pass           =>
      copy(
        actions = p :: actions,
        state = state.applyPass(p)
      )
    case ss: SelectSquares =>
      copy(
        actions = ss :: actions,
        state = state.applySelectSquares(ss)
      )
  }

  def addSettlement(selectSquares: SelectSquares): Replay =
    copy(
      actions = selectSquares :: actions,
      state = Replay.withSettlementCaptures(state.applySelectSquares(selectSquares), selectSquares)
    )

}

object Replay {

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
      actionStrs: ActionStrs,
      startPlayer: Player,
      activePlayer: Player,
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Validated[String, Reader.Result] = {
    val fen                            = initialFen.getOrElse(variant.initialFen)
    val (init, gameWithActions, error) =
      gameWithActionWhileValid(actionStrs, startPlayer, activePlayer, fen, variant)
    val game                           =
      gameWithActions.reverse.lastOption.map(_._1).getOrElse(init)

    error match {
      case None      =>
        Validated.valid(
          Reader.Result.Complete(
            new Replay(init, gameWithActions.reverse.map(_._2), game)
          )
        )
      case Some(msg) => Validated.invalid(msg)
    }
  }

  // TODO: because this is primarily used in a Validation context, we should be able to
  //       return something that's runtime safe as well.
  private def goAction(action: StratAction) = action match {
    case StratDrop.Go(d)           => d
    case StratPass.Go(p)           => p
    case StratSelectSquares.Go(ss) => ss
    case _                         => sys.error("Invalid go action")
  }

  def replayDrop(
      before: Game,
      role: Role,
      dest: Pos,
      endTurn: Boolean
  ): Drop =
    before.situation
      .drop(role, dest)
      .map(_.copy(autoEndTurn = endTurn))
      .valueOr(error =>
        sys.error(s"Illegal action ${role.forsyth}@${dest.key} at ply ${before.plies} for replay: ${error}")
      )

  def replayPass(before: Game, endTurn: Boolean): Pass =
    Pass(situationBefore = before.situation, autoEndTurn = endTurn)

  def replaySelectSquares(before: Game, squares: List[Pos], endTurn: Boolean): SelectSquares =
    SelectSquares(squares, situationBefore = before.situation, autoEndTurn = endTurn)

  private def withSettlementCaptures(played: Game, selectSquares: SelectSquares): Game =
    played.copy(situation = withSettlementCaptures(played.situation, selectSquares))

  private def withSettlementCaptures(played: Situation, selectSquares: SelectSquares): Situation =
    played.copy(board =
      played.board.updateHistory(history =>
        history.copy(captures =
          history.captures.add(
            selectSquares.player,
            settlementCaptureCount(selectSquares.before.pieces.size, selectSquares.after.pieces.size)
          )
        )
      )
    )

  // NOTE: the +1 counts one stone more than an `ss:` lifts. Preserved rather than fixed: the
  // `History.captures` of every settled game already stored downstream was written with it.
  private def settlementCaptureCount(stonesBefore: Int, stonesAfter: Int): Int =
    stonesBefore - stonesAfter + 1

  def actionStrsWithEndTurn(actionStrs: ActionStrs): Seq[(String, Boolean)] =
    actionStrs.zipWithIndex.map { case (a, i) =>
      a.zipWithIndex.map { case (a1, i1) => (a1, i1 == a.size - 1 && i != actionStrs.size - 1) }
    }.flatten

  private def combineActionStrsWithEndTurn(
      actionStrs: ActionStrs,
      startPlayer: Player,
      activePlayer: Player
  ): Seq[(String, Boolean)] =
    actionStrsWithEndTurn(
      if (Player.fromTurnCount(actionStrs.size + startPlayer.fold(0, 1)) == activePlayer)
        actionStrs :+ Vector()
      else actionStrs
    )

  private def gameWithActionWhileValid(
      actionStrs: ActionStrs,
      startPlayer: Player,
      activePlayer: Player,
      initialFen: FEN,
      variant: strategygames.go.variant.Variant
  ): (Game, List[(Game, Action)], Option[String]) = {

    val init   = makeGame(variant, initialFen.some)
    var state  = init
    var errors = ""

    def replayDropFromUci(
        role: Option[Role],
        dest: Option[Pos],
        endTurn: Boolean
    ): (Game, Action) =
      (role, dest) match {
        case (Some(role), Some(dest)) => {
          val drop = replayDrop(state, role, dest, endTurn)
          state = state.applyDrop(drop)
          (state, drop)
        }
        case (role, dest)             => {
          val uciDrop = s"${role}@${dest}"
          errors += uciDrop + ","
          sys.error(s"Invalid drop for replay: ${uciDrop}")
        }
      }

    def replayPassFromUci(endTurn: Boolean): (Game, Action) = {
      val pass = replayPass(state, endTurn)
      state = state.applyPass(pass)
      (state, pass)
    }

    def replaySelectSquaresFromUci(squares: List[Pos], endTurn: Boolean): (Game, Action) = {
      val selectSquares = replaySelectSquares(state, squares, endTurn)
      state = withSettlementCaptures(state.applySelectSquares(selectSquares), selectSquares)
      (state, selectSquares)
    }

    val gameWithActions: List[(Game, Action)] =
      combineActionStrsWithEndTurn(actionStrs, startPlayer, activePlayer).toList.map {
        case (actionStr, _) if state.situation.end           =>
          sys.error(s"Action ${actionStr} offered to a finished ${variant.key} game")
        case (Uci.Drop.dropR(role, dest), endTurn)           =>
          replayDropFromUci(
            Role.allByForsyth(init.situation.board.variant.gameFamily).get(role(0)),
            Pos.fromKey(dest),
            endTurn
          )
        case (Uci.Pass.passR(), endTurn)                     => replayPassFromUci(endTurn)
        case (Uci.SelectSquares.selectSquaresR(ss), endTurn) =>
          replaySelectSquaresFromUci(ss.split(",").toList.flatMap(Pos.fromKey(_)), endTurn)
        case (actionStr: String, _)                          =>
          sys.error(s"Invalid actionStr for replay: $actionStr")
      }

    (init, gameWithActions, errors match { case "" => None; case _ => errors.some })
  }

  def gameWithUciWhileValid(
      actionStrs: ActionStrs,
      startPlayer: Player,
      activePlayer: Player,
      initialFen: FEN,
      variant: strategygames.go.variant.Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[String]) = {
    val (game, gameWithActions, error) = gameWithActionWhileValid(
      actionStrs,
      startPlayer,
      activePlayer,
      initialFen,
      variant
    )
    (
      game,
      gameWithActions.map { v =>
        {
          val (state, action) = v
          (state, Uci.WithSan(Uci(action.toUci.uci).get, "NOSAN"))
        }
      },
      error
    )
  }

  private def recursiveSituations(sit: Situation, sans: List[San]): Validated[String, List[Situation]] =
    sans match {
      case Nil         => valid(Nil)
      case san :: rest =>
        san(StratSituation.wrap(sit)).map(goAction) andThen { action =>
          val after = Situation(action.finalizeAfter, !sit.player)
          recursiveSituations(after, rest) map { after :: _ }
        }
    }

  private def recursiveSituationsFromUci(
      sit: Situation,
      ucis: List[Uci]
  ): Validated[String, List[Situation]] =
    ucis match {
      case Nil         => valid(Nil)
      case uci :: rest =>
        uci(sit) andThen { action =>
          val after = Situation(action.finalizeAfter, !sit.player)
          recursiveSituationsFromUci(after, rest) map { after :: _ }
        }
    }

  private def recursiveReplayFromUci(replay: Replay, ucis: List[Uci]): Validated[String, Replay] =
    ucis match {
      case Nil         => valid(replay)
      case uci :: rest =>
        uci(replay.state.situation) andThen { action =>
          recursiveReplayFromUci(replay.addAction(action), rest)
        }
    }

  private def initialFenToSituation(
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Situation = {
    initialFen.flatMap(Forsyth.<<@(variant, _)) | Situation(variant)
  } withVariant variant

  def boards(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Validated[String, List[Board]] = situations(actionStrs, initialFen, variant) map (_ map (_.board))

  def situations(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    // seemingly this isn't used
    Parser.sans(actionStrs.flatten, sit.board.variant) andThen { sans =>
      recursiveSituations(sit, sans.value) map { sit :: _ }
    }
  }

  def boardsFromUci(
      ucis: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Validated[String, List[Board]] = situationsFromUci(ucis, initialFen, variant) map (_ map (_.board))

  def situationsFromUci(
      ucis: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    recursiveSituationsFromUci(sit, ucis) map { sit :: _ }
  }

  def gameFromUciStrings(
      uciStrings: ActionStrs,
      activePlayer: Player,
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Validated[String, Game] = {
    val fen                            = initialFen.getOrElse(variant.initialFen)
    val (init, gameWithActions, error) =
      gameWithActionWhileValid(uciStrings, fen.player.getOrElse(Player.P1), activePlayer, fen, variant)

    error match {
      case None      => Validated.valid(gameWithActions.lastOption.map(_._1).getOrElse(init))
      case Some(msg) => Validated.invalid(msg)
    }
  }

  def apply(
      ucis: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), ucis)

  private def makeGame(variant: strategygames.go.variant.Variant, initialFen: Option[FEN]): Game = {
    val g = Game(variant.some, initialFen)
    g.copy(startedAtPly = g.plies, startedAtTurn = g.turnCount)
  }
}
