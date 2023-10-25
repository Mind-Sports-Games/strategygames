package strategygames.chess

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._

import strategygames.Player
import strategygames.format.pgn.San
import strategygames.chess.format.pgn.{ Parser, Reader }
import strategygames.format.pgn.{ Tag, Tags }
import strategygames.chess.format.{ FEN, Forsyth, Uci }
import strategygames.{
  Action => StratAction,
  ActionStrs,
  Drop => StratDrop,
  Game => StratGame,
  Move => StratMove,
  Situation => StratSituation
}

case class Replay(setup: Game, plies: List[Action], state: Game) {

  lazy val chronoPlies = plies.reverse

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

  def addPly(action: Action) = action match {
    case m: Move =>
      copy(
        plies = m.applyVariantEffect :: plies,
        state = state.apply(m)
      )
    case d: Drop =>
      copy(
        plies = d :: plies,
        state = state.applyDrop(d)
      )
  }

}

object Replay {

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.chess.variant.Variant
  ): Validated[String, Reader.Result] =
    actionStrs.some.filter(_.nonEmpty) toValid "[replay] pgn is empty" andThen { nonEmptyActionStrs =>
      Reader.replayResult(
        nonEmptyActionStrs,
        Tags(
          List(
            initialFen map { fen =>
              Tag(_.FEN, fen.value)
            },
            variant.some.filterNot(_.standard) map { v =>
              Tag(_.Variant, v.name)
            }
          ).flatten
        )
      )
    }

  def chessAction(action: StratAction) = action match {
    case StratMove.Chess(m) => m
    case StratDrop.Chess(d) => d
    case _                  => sys.error("Invalid chess action")
  }

  // Both of the following commented out functions are unused by the rest of strategygames
  // and lila. Would need to upgrade to multiaction to use them
  //  private def recursiveGames(game: Game, sans: List[San]): Validated[String, List[Game]] =
  //  sans match {
  //    case Nil         => valid(Nil)
  //    case san :: rest =>
  //      san(StratSituation.wrap(game.situation)) flatMap { action =>
  //        val newGame = StratGame.wrap(game)(action).toChess
  //        recursiveGames(newGame, rest) map { newGame :: _ }
  //      }
  //  }

  // def games(
  //    moveStrs: Iterable[String],
  //    initialFen: Option[FEN],
  //    variant: strategygames.chess.variant.Variant
  // ): Validated[String, List[Game]] =
  //  Parser.moves(moveStrs, variant) andThen { moves =>
  //    val game = makeGame(variant, initialFen)
  //    recursiveGames(game, moves.value) map { game :: _ }
  //  }

  def gamePlyWhileValid(
      actionStrs: ActionStrs,
      initialFen: FEN,
      variant: strategygames.chess.variant.Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[String]) = {

    def mk(g: Game, plys: List[(San, String)]): (List[(Game, Uci.WithSan)], Option[String]) =
      plys match {
        case (san, sanStr) :: rest =>
          san(StratSituation.wrap(g.situation)).fold(
            err => (Nil, err.some),
            action => {
              val newGame = StratGame.wrap(g)(action).toChess
              val uci     = action.toUci.toChess
              mk(newGame, rest) match {
                case (next, msg) => ((newGame, Uci.WithSan(uci, sanStr)) :: next, msg)
              }
            }
          )
        case _                     => (Nil, None)
      }
    val init                                                                                = makeGame(variant, initialFen.some)
    // The following line converts actionStrs into a 1-dimensional structure
    // where an action is in a tuple of itself and the boolean autoEndTurn
    // actionStrs.zipWithIndex.map{case (a, i) => a.zipWithIndex.map{case (a1, i1) => (a1, i1 == a.size-1 && i != actionStrs.size-1)}}.flatten
    Parser
      // Its ok to flatten actionStrs as the game is built back up again from the Situation
      // If we don't want to flatten then we need to do something like samurai gamelogic
      // where we use startPlayer and activePlayer
      .sans(actionStrs.flatten, variant)
      .fold(
        err => List.empty[(Game, Uci.WithSan)] -> err.some,
        sans => mk(init, sans.value zip actionStrs.flatten)
      ) match {
      case (games, err) => (init, games, err)
    }
  }

  private def recursiveSituations(sit: Situation, sans: List[San]): Validated[String, List[Situation]] =
    sans match {
      case Nil         => valid(Nil)
      case san :: rest =>
        san(StratSituation.wrap(sit)).map(chessAction) flatMap { action =>
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
          recursiveReplayFromUci(replay addPly action, rest)
        }
    }

  private def initialFenToSituation(
      initialFen: Option[FEN],
      variant: strategygames.chess.variant.Variant
  ): Situation = {
    initialFen.flatMap(Forsyth.<<) | Situation(strategygames.chess.variant.Standard)
  } withVariant variant

  def boards(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.chess.variant.Variant
  ): Validated[String, List[Board]] = situations(actionStrs, initialFen, variant) map (_ map (_.board))

  def situations(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.chess.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    // Its ok to flatten actionStrs as the game is built back up again from the Situation
    Parser.sans(actionStrs.flatten, sit.board.variant) andThen { sans =>
      recursiveSituations(sit, sans.value) map { sit :: _ }
    }
  }

  def boardsFromUci(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.chess.variant.Variant
  ): Validated[String, List[Board]] = situationsFromUci(moves, initialFen, variant) map (_ map (_.board))

  def situationsFromUci(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.chess.variant.Variant
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

  def gameFromUciStrings(
      uciStrings: List[String],
      initialFen: Option[FEN],
      variant: strategygames.chess.variant.Variant
  ): Validated[String, Game] = {
    val init = makeGame(variant, initialFen)
    val ucis = uciStrings.flatMap(Uci.apply(_))
    if (uciStrings.size != ucis.size) invalid("Invalid Ucis")
    else recursiveGamesFromUci(init, ucis).map(_.last)
  }

  def apply(
      plies: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.chess.variant.Variant
  ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), plies)

  def plyAtFen(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: strategygames.chess.variant.Variant,
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
            san(StratSituation.wrap(sit)).map(chessAction) flatMap { action =>
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

      // Its ok to flatten actionStrs as the game is built back up again from the Situation
      Parser.sans(actionStrs.flatten, sit.board.variant) andThen { sans =>
        recursivePlyAtFen(sit, sans.value, 0, 0)
      }
    }

  private def makeGame(variant: strategygames.chess.variant.Variant, initialFen: Option[FEN]): Game = {
    val g = Game(variant.some, initialFen)
    g.copy(
      startedAtPlies = g.plies,
      startedAtTurn = g.turnCount
    )
  }
}
