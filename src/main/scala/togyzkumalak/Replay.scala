package strategygames.togyzkumalak

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._

import strategygames.format.pgn.San
import strategygames.togyzkumalak.format.pgn.{ Parser, Reader }
import strategygames.format.pgn.{ Tag, Tags }
import strategygames.togyzkumalak.format.{ FEN, Forsyth, Uci }
import strategygames.{ Actions, Move => StratMove, MoveOrDrop, Player, Situation => StratSituation }

case class Replay(setup: Game, moves: List[Move], state: Game) {

  lazy val chronoMoves = moves.reverse

  lazy val chronoActions: List[List[Move]] =
    chronoMoves
      .drop(1)
      .foldLeft(List(chronoMoves.take(1))) { case (turn, move) =>
        if (turn.head.head.situationBefore.player != move.situationBefore.player) {
          List(move) +: turn
        } else {
          (turn.head :+ move) +: turn.tail
        }
      }
      .reverse

  def addMove(move: Move) =
    copy(
      moves = move.applyVariantEffect :: moves,
      state = state.apply(move)
    )

  def moveAtPly(ply: Int): Option[Move] =
    chronoMoves lift (ply - 1 - setup.startedAtTurn)
}

object Replay {

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
      actions: Actions,
      initialFen: Option[FEN],
      variant: strategygames.togyzkumalak.variant.Variant
  ): Validated[String, Reader.Result] = {
    val fen                 = initialFen.getOrElse(variant.initialFen)
    val (init, plys, error) = gameActionWhileValid(actions, fen, variant)
    val game                = plys.reverse.last._1
    error match {
      case None      => Validated.valid(Reader.Result.Complete(new Replay(init, plys.reverse.map(_._2), game)))
      case Some(msg) => Validated.invalid(msg)
    }
  }

  // TODO: because this is primarily used in a Validation context, we should be able to
  //       return something that's runtime safe as well.
  def togyzkumalakMove(moveOrDrop: MoveOrDrop) = moveOrDrop match {
    case Left(StratMove.Togyzkumalak(m)) => m
    case _                               => sys.error("Invalid togyzkumalak move")
  }

  def replayMove(
      before: Game,
      orig: Pos,
      dest: Pos
  ): Move =
    Move(
      piece = before.situation.board.pieces(orig)._1,
      orig = orig,
      dest = dest,
      situationBefore = before.situation,
      after = before.situation.board.variant.boardAfter(before.situation, orig, dest),
      autoEndTurn = true,
      capture = None,
      promotion = None
    )

  private def gameActionWhileValid(
      actions: Actions,
      initialFen: FEN,
      variant: strategygames.togyzkumalak.variant.Variant
  ): (Game, List[(Game, Move)], Option[String]) = {
    val init   = makeGame(variant, initialFen.some)
    var state  = init
    var errors = ""

    def replayMoveFromUci(orig: Option[Pos], dest: Option[Pos], promotion: String): (Game, Move) =
      (orig, dest) match {
        case (Some(orig), Some(dest)) => {
          val move = replayMove(state, orig, dest)
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
      variant: strategygames.togyzkumalak.variant.Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[String]) = {
    val (game, plys, error) = gameActionWhileValid(actions, initialFen, variant)
    (
      game,
      plys.map { v =>
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
        san(StratSituation.wrap(sit)).map(togyzkumalakMove) flatMap { move =>
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
        uci(replay.state.situation) andThen { move =>
          recursiveReplayFromUci(replay.addMove(move), rest)
        }
    }

  private def initialFenToSituation(
      initialFen: Option[FEN],
      variant: strategygames.togyzkumalak.variant.Variant
  ): Situation = {
    initialFen.flatMap(Forsyth.<<) | Situation(strategygames.togyzkumalak.variant.Variant.default)
  } withVariant variant

  def boards(
      actions: Actions,
      initialFen: Option[FEN],
      variant: strategygames.togyzkumalak.variant.Variant
  ): Validated[String, List[Board]] = situations(actions, initialFen, variant) map (_ map (_.board))

  def situations(
      actions: Actions,
      initialFen: Option[FEN],
      variant: strategygames.togyzkumalak.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    Parser.moves(actions.flatten, sit.board.variant) andThen { moves =>
      recursiveSituations(sit, moves.value) map { sit :: _ }
    }
  }

  def boardsFromUci(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.togyzkumalak.variant.Variant
  ): Validated[String, List[Board]] = situationsFromUci(moves, initialFen, variant) map (_ map (_.board))

  def situationsFromUci(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.togyzkumalak.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    recursiveSituationsFromUci(sit, moves) map { sit :: _ }
  }

  def apply(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.togyzkumalak.variant.Variant
  ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), moves)

  def plyAtFen(
      actions: Actions,
      initialFen: Option[FEN],
      variant: strategygames.togyzkumalak.variant.Variant,
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
            san(StratSituation.wrap(sit)).map(togyzkumalakMove) flatMap { move =>
              val after = move.finalizeAfter
              val fen   = Forsyth >> Game(Situation(after, Player.fromPly(ply)), turns = ply)
              if (compareFen(fen)) Validated.valid(ply)
              else recursivePlyAtFen(Situation(after, !sit.player), rest, ply + 1)
            }
        }

      val sit = initialFen.flatMap {
        Forsyth.<<@(variant, _)
      } | Situation(variant)

      Parser.moves(actions.flatten, sit.board.variant) andThen { moves =>
        recursivePlyAtFen(sit, moves.value, 1)
      }
    }

  private def makeGame(variant: strategygames.togyzkumalak.variant.Variant, initialFen: Option[FEN]): Game = {
    val g = Game(variant.some, initialFen)
    g.copy(startedAtTurn = g.turns, startPlayer = g.situation.player)
  }
}
