package strategygames.mancala

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._

import strategygames.Player
import strategygames.format.pgn.San
import strategygames.mancala.format.pgn.{ Parser, Reader }
import strategygames.format.pgn.{ Tag, Tags }
import strategygames.mancala.format.{ FEN, Forsyth, Uci }
import strategygames.{ Move => StratMove, MoveOrDrop, Situation => StratSituation }

case class Replay(setup: Game, moves: List[Move], state: Game) {

  lazy val chronoMoves = moves.reverse

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
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: strategygames.mancala.variant.Variant
  ): Validated[String, Reader.Result] =
    moveStrs.some.filter(_.nonEmpty) toValid "[replay] pgn is empty" andThen { nonEmptyMoves =>
      Reader.moves(
        nonEmptyMoves,
        Tags(
          List(
            initialFen map { fen =>
              Tag(_.FEN, fen.value)
            },
            variant.some.filterNot(_.oware) map { v =>
              Tag(_.Variant, v.name)
            }
          ).flatten
        )
      )
    }

  // TODO: because this is primarily used in a Validation context, we should be able to
  //       return something that's runtime safe as well.
  def mancalaMove(moveOrDrop: MoveOrDrop) = moveOrDrop match {
    case Left(StratMove.Mancala(m)) => m
    case _                          => sys.error("Invalid mancala move")
  }

  def replayMove(
      before: Game,
      orig: Pos,
      dest: Pos,
      apiPosition: Api.Position,
      uciMoves: List[String]
  ): Move =
    Move(
      piece = before.situation.board.pieces(orig),
      orig = orig,
      dest = dest,
      situationBefore = before.situation,
      after = before.situation.board.copy(
        pieces = apiPosition.pieceMap,
        uciMoves = uciMoves,
        position = apiPosition.some
      ),
      capture = None,
      promotion = None
    )

  def gameMoveWhileValid(
      moveStrs: Seq[String],
      initialFen: FEN,
      variant: strategygames.mancala.variant.Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[String]) = {

    val init     = makeGame(variant, initialFen.some)
    var state    = init
    var uciMoves = init.situation.board.uciMoves
    var errors   = ""

    def getApiPosition(uciMoves: List[String]) =
      Api.positionFromVariantAndMoves(variant, uciMoves)

    def replayMoveFromUci(orig: Option[Pos], dest: Option[Pos], promotion: String): (Game, Uci.WithSan) =
      (orig, dest) match {
        case (Some(orig), Some(dest)) => {
          val uciMove = s"${orig.key}${dest.key}${promotion}"
          val pgnMove = s"${orig.key}${dest.key}${promotion match {
              case "" => ""
              case _  => state.board.pieces(orig).role.forsyth
            }}"
          uciMoves = uciMoves :+ uciMove
          state = state.apply(
            replayMove(state, orig, dest, getApiPosition(uciMoves), uciMoves)
          )
          (state, Uci.WithSan(Uci(pgnMove).get, "NOSAN"))
        }
        case (orig, dest)             => {
          val uciMove = s"${orig}${dest}${promotion}"
          errors += uciMove + ","
          sys.error(s"Invalid move for replay: ${uciMove}")
        }
      }

    val moves: List[(Game, Uci.WithSan)] = Parser
      .pgnMovesToUciMoves(moveStrs)
      .map {
        case Uci.Move.moveR(orig, dest, promotion) =>
          replayMoveFromUci(
            Pos.fromKey(orig),
            Pos.fromKey(dest),
            promotion
          )
        case moveStr: String                       => sys.error(s"Invalid move for replay: $moveStr")
      }

    (init, moves, errors match { case "" => None; case _ => errors.some })
  }

  private def recursiveSituations(sit: Situation, sans: List[San]): Validated[String, List[Situation]] =
    sans match {
      case Nil         => valid(Nil)
      case san :: rest =>
        san(StratSituation.wrap(sit)).map(mancalaMove) flatMap { move =>
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
      variant: strategygames.mancala.variant.Variant
  ): Situation = {
    initialFen.flatMap(Forsyth.<<) | Situation(strategygames.mancala.variant.Variant.default)
  } withVariant variant

  def boards(
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: strategygames.mancala.variant.Variant
  ): Validated[String, List[Board]] = situations(moveStrs, initialFen, variant) map (_ map (_.board))

  def situations(
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: strategygames.mancala.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    Parser.moves(moveStrs, sit.board.variant) andThen { moves =>
      recursiveSituations(sit, moves.value) map { sit :: _ }
    }
  }

  def boardsFromUci(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.mancala.variant.Variant
  ): Validated[String, List[Board]] = situationsFromUci(moves, initialFen, variant) map (_ map (_.board))

  def situationsFromUci(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.mancala.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    recursiveSituationsFromUci(sit, moves) map { sit :: _ }
  }

  def apply(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.mancala.variant.Variant
  ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), moves)

  def plyAtFen(
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: strategygames.mancala.variant.Variant,
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
            san(StratSituation.wrap(sit)).map(mancalaMove) flatMap { move =>
              val after = move.finalizeAfter
              val fen   = Forsyth >> Game(Situation(after, Player.fromPly(ply)), turns = ply)
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

  private def makeGame(variant: strategygames.mancala.variant.Variant, initialFen: Option[FEN]): Game = {
    val g = Game(variant.some, initialFen)
    g.copy(startedAtTurn = g.turns)
  }
}
