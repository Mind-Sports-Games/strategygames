package strategygames.samurai

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._

import strategygames.format.pgn.San
import strategygames.samurai.format.pgn.{ Parser, Reader }
import strategygames.format.pgn.{ Tag, Tags }
import strategygames.samurai.format.{ FEN, Forsyth, Uci }
import strategygames.{ Actions, Move => StratMove, MoveOrDrop, Situation => StratSituation }

case class Replay(setup: Game, plies: List[Move], state: Game) {

  lazy val chronoPlies = plies.reverse

  lazy val chronoActions: List[List[Move]] =
    chronoPlies
      .drop(1)
      .foldLeft(List(chronoPlies.take(1))) { case (turn, move) =>
        if (turn.head.head.situationBefore.player != move.situationBefore.player) {
          List(move) +: turn
        } else {
          (turn.head :+ move) +: turn.tail
        }
      }
      .reverse

  def addPly(move: Move) =
    copy(
      plies = move.applyVariantEffect :: plies,
      state = state.apply(move)
    )

}

object Replay {

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
      actions: Actions,
      initialFen: Option[FEN],
      variant: strategygames.samurai.variant.Variant
  ): Validated[String, Reader.Result] = {
    val fen                  = initialFen.getOrElse(variant.initialFen)
    val (init, plies, error) = gameActionWhileValid(actions, fen, variant)
    val game                 = plies.reverse.last._1
    error match {
      case None      => Validated.valid(Reader.Result.Complete(new Replay(init, plies.reverse.map(_._2), game)))
      case Some(msg) => Validated.invalid(msg)
    }
  }

  // TODO: because this is primarily used in a Validation context, we should be able to
  //       return something that's runtime safe as well.
  def samuraiMove(moveOrDrop: MoveOrDrop) = moveOrDrop match {
    case Left(StratMove.Samurai(m)) => m
    case _                          => sys.error("Invalid samurai move")
  }

  def replayMove(
      before: Game,
      orig: Pos,
      dest: Pos,
      apiPosition: Api.Position,
      uciMoves: List[String]
  ): Move =
    Move(
      piece = before.situation.board.pieces(orig)._1,
      orig = orig,
      dest = dest,
      situationBefore = before.situation,
      after = before.situation.board.copy(
        pieces = apiPosition.pieceMap,
        uciMoves = uciMoves,
        position = apiPosition.some
      ),
      autoEndTurn = true,
      capture = None,
      promotion = None
    )

  private def gameActionWhileValid(
      actions: Actions,
      initialFen: FEN,
      variant: strategygames.samurai.variant.Variant
  ): (Game, List[(Game, Move)], Option[String]) = {
    val init     = makeGame(variant, initialFen.some)
    var state    = init
    var uciMoves = init.situation.board.uciMoves
    var errors   = ""

    def getApiPosition(uciMoves: List[String]) =
      Api.positionFromVariantAndMoves(variant, uciMoves)

    def replayMoveFromUci(orig: Option[Pos], dest: Option[Pos], promotion: String): (Game, Move) =
      (orig, dest) match {
        case (Some(orig), Some(dest)) => {
          val uciMove = s"${orig.key}${dest.key}${promotion}"
          uciMoves = uciMoves :+ uciMove
          val move    = replayMove(state, orig, dest, getApiPosition(uciMoves), uciMoves)
          state = state(move)
          (state, move)
        }
        case (orig, dest)             => {
          val uciMove = s"${orig}${dest}${promotion}"
          errors += uciMove + ","
          sys.error(s"Invalid move for replay: ${uciMove}")
        }
      }

    val plys: List[(Game, Move)] = Parser
      .pgnMovesToUciMoves(actions.flatten)
      .map {
        case Uci.Move.moveR(orig, dest, promotion) =>
          replayMoveFromUci(
            Pos.fromKey(orig),
            Pos.fromKey(dest),
            promotion
          )
        case action: String                        => sys.error(s"Invalid move for replay: $action")
      }

    (init, plys, errors match { case "" => None; case _ => errors.some })
  }

  def gamePlyWhileValid(
      actions: Actions,
      initialFen: FEN,
      variant: strategygames.samurai.variant.Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[String]) = {
    val (game, moves, error) = gameActionWhileValid(actions, initialFen, variant)
    (
      game,
      moves.map { v =>
        {
          val (state, move) = v
          (state, Uci.WithSan(move.toUci, "NOSAN"))
        }
      },
      error
    )
  }

  private def recursiveSituations(sit: Situation, sans: List[San]): Validated[String, List[Situation]] =
    sans match {
      case Nil         => valid(Nil)
      case san :: rest =>
        san(StratSituation.wrap(sit)).map(samuraiMove) flatMap { move =>
          val after = Situation(move.finalizeAfter, !sit.player)
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
        uci(sit) andThen { move =>
          val after = Situation(move.finalizeAfter, !sit.player)
          recursiveSituationsFromUci(after, rest) map { after :: _ }
        }
    }

  private def recursiveReplayFromUci(replay: Replay, ucis: List[Uci]): Validated[String, Replay] =
    ucis match {
      case Nil         => valid(replay)
      case uci :: rest =>
        uci(replay.state.situation) andThen { ply =>
          recursiveReplayFromUci(replay.addPly(ply), rest)
        }
    }

  private def initialFenToSituation(
      initialFen: Option[FEN],
      variant: strategygames.samurai.variant.Variant
  ): Situation = {
    initialFen.flatMap(Forsyth.<<) | Situation(strategygames.samurai.variant.Variant.default)
  } withVariant variant

  def boards(
      actions: Actions,
      initialFen: Option[FEN],
      variant: strategygames.samurai.variant.Variant
  ): Validated[String, List[Board]] = situations(actions, initialFen, variant) map (_ map (_.board))

  def situations(
      actions: Actions,
      initialFen: Option[FEN],
      variant: strategygames.samurai.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    Parser.moves(actions.flatten, sit.board.variant) andThen { moves =>
      recursiveSituations(sit, moves.value) map { sit :: _ }
    }
  }

  def boardsFromUci(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.samurai.variant.Variant
  ): Validated[String, List[Board]] = situationsFromUci(moves, initialFen, variant) map (_ map (_.board))

  def situationsFromUci(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.samurai.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    recursiveSituationsFromUci(sit, moves) map { sit :: _ }
  }

  def apply(
      plies: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.samurai.variant.Variant
  ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), plies)

  def plyAtFen(
      actions: Actions,
      initialFen: Option[FEN],
      variant: strategygames.samurai.variant.Variant,
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
            san(StratSituation.wrap(sit)).map(samuraiMove) flatMap { move =>
              val after = move.situationAfter
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

  private def makeGame(variant: strategygames.samurai.variant.Variant, initialFen: Option[FEN]): Game = {
    val g = Game(variant.some, initialFen)
    g.copy(startedAtPly = g.turns, startPlayer = g.situation.player)
  }
}
