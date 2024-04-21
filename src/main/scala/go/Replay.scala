package strategygames.go

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._

import strategygames.{ Player, Score }
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
      endTurn: Boolean,
      apiPosition: Api.Position,
      uciMoves: List[String]
  ): Drop = {
    val piece = Piece(before.situation.player, role)
    Drop(
      piece = piece,
      pos = dest,
      situationBefore = before.situation,
      genNextBoard = LazyBoardAfter(() =>
        before.situation.board
          .copy(
            pieces = apiPosition.pieceMap,
            uciMoves = uciMoves,
            pocketData = apiPosition.pocketData,
            position = apiPosition.some
          )
          .withHistory(
            before.situation.history.copy(
              // lastTurn handled in Action.finalizeAfter
              score = Score(
                apiPosition.fen.player1Score,
                apiPosition.fen.player2Score
              ),
              captures = before.situation.history.captures.add(
                before.situation.player,
                before.situation.board.apiPosition.pieceMap.size - apiPosition.pieceMap.size + 1
              ),
              halfMoveClock = before.situation.history.halfMoveClock + before.situation.player.fold(0, 1)
            )
          )
      ),
      autoEndTurn = endTurn
    )
  }

  def replayPass(
      before: Game,
      endTurn: Boolean,
      apiPosition: Api.Position,
      uciMoves: List[String]
  ): Pass = {
    Pass(
      situationBefore = before.situation,
      after = before.situation.board
        .copy(
          pieces = apiPosition.pieceMap,
          uciMoves = uciMoves,
          pocketData = apiPosition.pocketData,
          position = apiPosition.some
        )
        .withHistory(
          before.situation.history.copy(
            // lastTurn handled in Action.finalizeAfter
            halfMoveClock = before.situation.history.halfMoveClock + before.situation.player.fold(0, 1)
          )
        ),
      autoEndTurn = endTurn
    )
  }

  def replaySelectSquares(
      before: Game,
      squares: List[Pos],
      endTurn: Boolean,
      apiPosition: Api.Position,
      uciMoves: List[String]
  ): SelectSquares = {
    SelectSquares(
      squares,
      situationBefore = before.situation,
      after = before.situation.board
        .copy(
          pieces = apiPosition.pieceMap,
          uciMoves = uciMoves,
          pocketData = apiPosition.pocketData,
          position = apiPosition.some
        )
        .withHistory(
          before.situation.history.copy(
            // lastTurn handled in Action.finalizeAfter
            score = Score(
              apiPosition.fen.player1Score,
              apiPosition.fen.player2Score
            ),
            captures = before.situation.history.captures.add(
              before.situation.player,
              before.situation.board.apiPosition.pieceMap.size - apiPosition.pieceMap.size + 1
            ),
            halfMoveClock = before.situation.history.halfMoveClock + before.situation.player.fold(0, 1)
          )
        ),
      autoEndTurn = endTurn
    )
  }

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

    val init     = makeGame(variant, initialFen.some)
    var state    = init
    var uciMoves = init.situation.board.uciMoves
    var errors   = ""

    def getApiPosition(uciMoves: List[String]) =
      Api.positionFromStartingFenAndMoves(initialFen, uciMoves)

    def replayDropFromUci(
        role: Option[Role],
        dest: Option[Pos],
        endTurn: Boolean
    ): (Game, Action) =
      (role, dest) match {
        case (Some(role), Some(dest)) => {
          val uciDrop = s"${role.forsyth}@${dest.key}"
          uciMoves = uciMoves :+ uciDrop
          val drop    = replayDrop(state, role, dest, endTurn, getApiPosition(uciMoves), uciMoves)
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
      uciMoves = uciMoves :+ "pass"
      val pass = replayPass(state, endTurn, getApiPosition(uciMoves), uciMoves)
      state = state.applyPass(pass)
      (state, pass)
    }

    def replaySelectSquaresFromUci(squares: List[Pos], endTurn: Boolean): (Game, Action) = {
      uciMoves = uciMoves :+ s"ss:${squares.mkString(",")}"
      val selectSquares = replaySelectSquares(state, squares, endTurn, getApiPosition(uciMoves), uciMoves)
      state = state.applySelectSquares(selectSquares)
      (state, selectSquares)
    }

    val gameWithActions: List[(Game, Action)] =
      combineActionStrsWithEndTurn(actionStrs, startPlayer, activePlayer).toList.map {
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
        san(StratSituation.wrap(sit)).map(goAction) flatMap { action =>
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
    initialFen.flatMap(Forsyth.<<) | Situation(variant)
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

  private def recursiveGamesFromUci(
      game: Game,
      ucis: List[Uci]
  ): Validated[String, List[Game]] =
    ucis match {
      case Nil         => valid(List(game))
      case uci :: rest =>
        game.apply(uci) andThen { case (game, _) =>
          recursiveGamesFromUci(game, rest) map { game :: _ }
        }
    }

  // This mirrors the gameFromUciStrings implementation for other game logics but its slow
  def gameFromUciStringsSlow(
      uciStrings: List[String],
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Validated[String, Game] = {
    val init = makeGame(variant, initialFen)
    val ucis = uciStrings.flatMap(Uci.apply(_))
    if (uciStrings.size != ucis.size) invalid("Invalid Ucis")
    else recursiveGamesFromUci(init, ucis).map(_.last)
  }

  // this is a fast implementation which we can use because 'uci' is the only format we use
  def gameFromUciStrings(
      uciStrings: ActionStrs,
      activePlayer: Player,
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Validated[String, Game] = {
    val fen = initialFen.getOrElse(variant.initialFen)
    val r   = gameWithActionWhileValid(
      uciStrings,
      fen.player.getOrElse(Player.P1),
      activePlayer,
      fen,
      variant
    )
    if (uciStrings.size > 0) valid(r._2.last._1)
    else valid(r._1)
  }

  def apply(
      ucis: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), ucis)

  def plyAtFen(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant,
      atFen: FEN
  ): Validated[String, Int] =
    if (Forsyth.<<@(variant, atFen).isEmpty) invalid(s"Invalid FEN $atFen")
    else {

      // we don't want to compare the full move number, to match transpositions
      def truncateFen(fen: FEN) = fen.value.split(' ').take(4) mkString " "
      val atFenTruncated        = truncateFen(atFen)
      def compareFen(fen: FEN)  = truncateFen(fen) == atFenTruncated

      def recursivePlyAtFen(sit: Situation, sans: List[San], ply: Int, turn: Int): Validated[String, Int] =
        sans match {
          case Nil         => invalid(s"Can't find $atFenTruncated, reached ply $ply, turn $turn")
          case san :: rest =>
            san(StratSituation.wrap(sit)).map(goAction) flatMap { action =>
              val after        = action.situationAfter
              val newPlies     = ply + 1
              val newTurnCount = turn + (if (sit.player != after.player) 1 else 0)
              val fen          = Forsyth >> Game(after, plies = newPlies, turnCount = newTurnCount)
              if (compareFen(fen)) Validated.valid(ply)
              else recursivePlyAtFen(after, rest, newPlies, newTurnCount)
            }
        }

      val sit = initialFen.flatMap {
        Forsyth.<<@(variant, _)
      } | Situation(variant)

      // seemingly this isn't used
      Parser.sans(actionStrs.flatten, sit.board.variant) andThen { sans =>
        recursivePlyAtFen(sit, sans.value, 0, 0)
      }
    }

  private def makeGame(variant: strategygames.go.variant.Variant, initialFen: Option[FEN]): Game = {
    val g = Game(variant.some, initialFen)
    g.copy(startedAtPly = g.plies, startedAtTurn = g.turnCount)
  }
}
