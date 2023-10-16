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
  Drop => StratDrop,
  Pass => StratPass,
  SelectSquares => StratSelectSquares,
  Situation => StratSituation
}

case class Replay(setup: Game, moves: List[Action], state: Game) {

  lazy val chronoMoves = moves.reverse

  def addMove(action: Action) = action match {
    case d: Drop           =>
      copy(
        moves = d.applyVariantEffect :: moves,
        state = state.applyDrop(d)
      )
    case p: Pass           =>
      copy(
        moves = p :: moves,
        state = state.applyPass(p)
      )
    case ss: SelectSquares =>
      copy(
        moves = ss :: moves,
        state = state.applySelectSquares(ss)
      )
  }

  def moveAtPly(ply: Int): Option[Action] =
    chronoMoves lift (ply - 1 - setup.startedAtTurn)
}

object Replay {

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Validated[String, Reader.Result] = {
    val fen                  = initialFen.getOrElse(variant.initialFen)
    val (init, moves, error) = gameMoveWhileValid__impl(moveStrs.toSeq, fen, variant)
    val game                 = moves.reverse.last._1
    error match {
      case None      => Validated.valid(Reader.Result.Complete(new Replay(init, moves.reverse.map(_._2), game)))
      case Some(msg) => Validated.invalid(msg)
    }
  }

  // TODO: because this is primarily used in a Validation context, we should be able to
  //       return something that's runtime safe as well.
  def goAction(action: StratAction) = action match {
    case StratDrop.Go(d)           => d
    case StratPass.Go(p)           => p
    case StratSelectSquares.Go(ss) => ss
    case _                         => sys.error("Invalid go action")
  }

  def replayDrop(
      before: Game,
      role: Role,
      dest: Pos,
      apiPosition: Api.Position,
      uciMoves: List[String]
  ): Drop = {
    val piece = Piece(before.situation.player, role)
    Drop(
      piece = piece,
      pos = dest,
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
            lastMove = Uci.Drop(role, dest).some,
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
    )
  }

  def replayPass(
      before: Game,
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
            lastMove = Uci.Pass().some,
            halfMoveClock = before.situation.history.halfMoveClock + before.situation.player.fold(0, 1)
          )
        )
    )
  }

  def replaySelectSquares(
      before: Game,
      squares: List[Pos],
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
            lastMove = Uci.SelectSquares(squares).some,
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
    )
  }

  private def gameMoveWhileValid__impl(
      moveStrs: Seq[String],
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
        prevStr: Option[String]
    ): (Game, Action) =
      (role, dest, prevStr) match {
        case (Some(role), Some(dest), None) => {
          val uciDrop = s"${role.forsyth}@${dest.key}"
          uciMoves = uciMoves :+ uciDrop
          val drop    = replayDrop(state, role, dest, getApiPosition(uciMoves), uciMoves)
          state = state.applyDrop(drop)
          (state, drop)
        }
        case (role, dest, _)                => {
          val uciDrop = s"${role}@${dest}"
          errors += uciDrop + ","
          sys.error(s"Invalid drop for replay: ${uciDrop}")
        }
      }

    def replayPassFromUci: (Game, Action) = {
      uciMoves = uciMoves :+ "pass"
      val pass = replayPass(state, getApiPosition(uciMoves), uciMoves)
      state = state.applyPass(pass)
      (state, pass)
    }

    def replaySelectSquaresFromUci(squares: List[Pos]): (Game, Action) = {
      uciMoves = uciMoves :+ s"ss:${squares.mkString(",")}"
      val selectSquares = replaySelectSquares(state, squares, getApiPosition(uciMoves), uciMoves)
      state = state.applySelectSquares(selectSquares)
      (state, selectSquares)
    }

    def parseActionWithPrevious(moveStr: String, prevStr: Option[String]): (Game, Action) =
      moveStr match {
        case Uci.Drop.dropR(role, dest)           =>
          replayDropFromUci(
            Role.allByForsyth(init.situation.board.variant.gameFamily).get(role(0)),
            Pos.fromKey(dest),
            prevStr
          )
        case Uci.Pass.passR()                     => replayPassFromUci
        case Uci.SelectSquares.selectSquaresR(ss) =>
          replaySelectSquaresFromUci(ss.split(",").toList.flatMap(Pos.fromKey(_)))
        case moveStr: String                      => sys.error(s"Invalid action for replay: $moveStr")
      }

    def parseAction(moveStr: String): (Game, Action) =
      parseActionWithPrevious(moveStr, None)

    def moves: List[(Game, Action)] =
      Parser.pgnMovesToUciMoves(moveStrs).map(parseAction)

    (init, moves, errors match { case "" => None; case _ => errors.some })
  }

  def gameMoveWhileValid(
      moveStrs: Seq[String],
      initialFen: FEN,
      variant: strategygames.go.variant.Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[String]) = {
    val (game, moves, error) = gameMoveWhileValid__impl(moveStrs, initialFen, variant)
    (
      game,
      moves.map { v =>
        {
          val (state, drop) = v
          (state, Uci.WithSan(Uci(drop.toUci.uci).get, "NOSAN"))
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
          recursiveReplayFromUci(replay addMove action, rest)
        }
    }

  private def initialFenToSituation(
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Situation = {
    initialFen.flatMap(Forsyth.<<) | Situation(variant)
  } withVariant variant

  def boards(
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Validated[String, List[Board]] = situations(moveStrs, initialFen, variant) map (_ map (_.board))

  def situations(
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    Parser.moves(moveStrs, sit.board.variant) andThen { moves =>
      recursiveSituations(sit, moves.value) map { sit :: _ }
    }
  }

  def boardsFromUci(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Validated[String, List[Board]] = situationsFromUci(moves, initialFen, variant) map (_ map (_.board))

  def situationsFromUci(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    recursiveSituationsFromUci(sit, moves) map { sit :: _ }
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
      uciStrings: List[String],
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Validated[String, Game] = {
    val r = gameMoveWhileValid(uciStrings, initialFen.getOrElse(variant.initialFen), variant)
    if (uciStrings.size > 0) valid(r._2.last._1)
    else valid(r._1)
  }

  def apply(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.go.variant.Variant
  ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), moves)

  def plyAtFen(
      moveStrs: Iterable[String],
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

      def recursivePlyAtFen(sit: Situation, sans: List[San], ply: Int): Validated[String, Int] =
        sans match {
          case Nil         => invalid(s"Can't find $atFenTruncated, reached ply $ply")
          case san :: rest =>
            san(StratSituation.wrap(sit)).map(goAction) flatMap { action =>
              val after = action.finalizeAfter
              val fen   =
                Forsyth >> Game(Situation(after, Player.fromPly(ply, variant.plysPerTurn)), turns = ply)
              if (compareFen(fen)) Validated.valid(ply)
              else recursivePlyAtFen(Situation(after, !sit.player), rest, ply + 1)
            }
        }

      val sit = initialFen.flatMap {
        Forsyth.<<@(variant, _)
      } | Situation(variant)

      Parser.moves(moveStrs, sit.board.variant) andThen { moves =>
        recursivePlyAtFen(sit, moves.value, 1)
      }
    }

  private def makeGame(variant: strategygames.go.variant.Variant, initialFen: Option[FEN]): Game = {
    val g = Game(variant.some, initialFen)
    g.copy(startedAtTurn = g.turns)
  }
}
