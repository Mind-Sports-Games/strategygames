package strategygames.chess

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._

import strategygames.format.pgn.San
import strategygames.chess.format.pgn.{ Parser, Reader }
import strategygames.format.pgn.{ Tag, Tags }
import strategygames.chess.format.{ FEN, Forsyth, Uci }
import strategygames.{ Actions, Game => StratGame, Situation => StratSituation }

case class Replay(setup: Game, plies: List[MoveOrDrop], state: Game) {

  lazy val chronoPlies = plies.reverse

  lazy val chronoActions: List[List[MoveOrDrop]] =
    chronoPlies
      .drop(1)
      .foldLeft(List(chronoPlies.take(1))) { case (turn, mod) =>
        if (
          turn.head.head.fold(_.situationBefore.player, _.situationBefore.player) != mod.fold(
            _.situationBefore.player,
            _.situationBefore.player
          )
        ) {
          List(mod) +: turn
        } else {
          (turn.head :+ mod) +: turn.tail
        }
      }
      .reverse

  def addPly(moveOrDrop: MoveOrDrop) =
    copy(
      plies = moveOrDrop.left.map(_.applyVariantEffect) :: plies,
      state = moveOrDrop.fold(state.apply, state.applyDrop)
    )

}

object Replay {

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
      actions: Actions,
      initialFen: Option[FEN],
      variant: strategygames.chess.variant.Variant
  ): Validated[String, Reader.Result] =
    actions.some.filter(_.nonEmpty) toValid "[replay] pgn is empty" andThen { nonEmptyMoves =>
      Reader.moves(
        nonEmptyMoves.flatten,
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

  private def recursiveGames(game: Game, sans: List[San]): Validated[String, List[Game]] =
    sans match {
      case Nil         => valid(Nil)
      case san :: rest =>
        san(StratSituation.wrap(game.situation)) flatMap { moveOrDrop =>
          val newGame = StratGame.wrap(game)(moveOrDrop).toChess
          recursiveGames(newGame, rest) map { newGame :: _ }
        }
    }

  def games(
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: strategygames.chess.variant.Variant
  ): Validated[String, List[Game]] =
    Parser.moves(moveStrs, variant) andThen { moves =>
      val game = makeGame(variant, initialFen)
      recursiveGames(game, moves.value) map { game :: _ }
    }

  def gamePlyWhileValid(
      actions: Actions,
      initialFen: FEN,
      variant: strategygames.chess.variant.Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[String]) = {

    def mk(g: Game, plys: List[(San, String)]): (List[(Game, Uci.WithSan)], Option[String]) =
      plys match {
        case (san, sanStr) :: rest =>
          san(StratSituation.wrap(g.situation)).fold(
            err => (Nil, err.some),
            moveOrDrop => {
              val newGame = StratGame.wrap(g)(moveOrDrop).toChess
              val uci     = moveOrDrop.fold(m => m.toUci.toChess, d => d.toUci.toChess)
              mk(newGame, rest) match {
                case (next, msg) => ((newGame, Uci.WithSan(uci, sanStr)) :: next, msg)
              }
            }
          )
        case _                     => (Nil, None)
      }
    val init                                                                                = makeGame(variant, initialFen.some)
    // TODO handle multimove
    // The following line converts actions into a 1-dimensional structure
    // where an action is in a tuple of itself and the boolean autoEndTurn
    // actions.zipWithIndex.map{case (a, i) => a.zipWithIndex.map{case (a1, i1) => (a1, i1 == a.size-1 && i != actions.size-1)}}.flatten
    Parser
      .moves(actions.flatten, variant)
      .fold(
        err => List.empty[(Game, Uci.WithSan)] -> err.some,
        moves => mk(init, moves.value zip actions.flatten)
      ) match {
      case (games, err) => (init, games, err)
    }
  }

  private def recursiveSituations(sit: Situation, sans: List[San]): Validated[String, List[Situation]] =
    sans match {
      case Nil         => valid(Nil)
      case san :: rest =>
        san(StratSituation.wrap(sit)) flatMap { moveOrDrop =>
          val after = Situation(
            moveOrDrop.fold(m => m.finalizeAfter().toChess, d => d.finalizeAfter.toChess),
            !sit.player
          )
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
        uci(sit) andThen { moveOrDrop =>
          val after = Situation(moveOrDrop.fold(_.finalizeAfter, _.finalizeAfter), !sit.player)
          recursiveSituationsFromUci(after, rest) map { after :: _ }
        }
    }

  private def recursiveReplayFromUci(replay: Replay, ucis: List[Uci]): Validated[String, Replay] =
    ucis match {
      case Nil         => valid(replay)
      case uci :: rest =>
        uci(replay.state.situation) andThen { ply =>
          recursiveReplayFromUci(replay addPly ply, rest)
        }
    }

  private def initialFenToSituation(
      initialFen: Option[FEN],
      variant: strategygames.chess.variant.Variant
  ): Situation = {
    initialFen.flatMap(Forsyth.<<) | Situation(strategygames.chess.variant.Standard)
  } withVariant variant

  def boards(
      actions: Actions,
      initialFen: Option[FEN],
      variant: strategygames.chess.variant.Variant
  ): Validated[String, List[Board]] = situations(actions, initialFen, variant) map (_ map (_.board))

  def situations(
      actions: Actions,
      initialFen: Option[FEN],
      variant: strategygames.chess.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    Parser.moves(actions.flatten, sit.board.variant) andThen { moves =>
      recursiveSituations(sit, moves.value) map { sit :: _ }
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

  def apply(
      plies: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.chess.variant.Variant
  ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), plies)

  def plyAtFen(
      actions: Actions,
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

      def recursivePlyAtFen(sit: Situation, sans: List[San], ply: Int): Validated[String, Int] =
        sans match {
          case Nil         => invalid(s"Can't find $atFenTruncated, reached ply $ply")
          case san :: rest =>
            san(StratSituation.wrap(sit)) flatMap { moveOrDrop =>
              val after = moveOrDrop.fold(m => m.situationAfter.toChess, d => d.situationAfter.toChess)
              val fen   = Forsyth >> Game(after, turns = ply)
              if (compareFen(fen)) Validated.valid(ply)
              else recursivePlyAtFen(after, rest, ply + 1)
            }
        }

      val sit = initialFen.flatMap {
        Forsyth.<<@(variant, _)
      } | Situation(variant)

      Parser.moves(actions.flatten, sit.board.variant) andThen { moves =>
        recursivePlyAtFen(sit, moves.value, 1)
      }
    }

  private def makeGame(variant: strategygames.chess.variant.Variant, initialFen: Option[FEN]): Game = {
    val g = Game(variant.some, initialFen)
    //TODO this only works for multiaction if turns is turns (not plies)
    g.copy(startedAtTurn = g.turns, startPlayer = g.situation.player)
  }
}
