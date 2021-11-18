package strategygames.fairysf

import cats.data.Validated
import cats.data.Validated.{ invalid, valid }
import cats.implicits._

import strategygames.Color
import strategygames.format.pgn.San
import strategygames.fairysf.format.pgn.{ Parser, Reader }
import strategygames.format.pgn.{ Tag, Tags }
import strategygames.fairysf.format.{ FEN, Forsyth, Uci }
import strategygames.{ Game => StratGame, Situation => StratSituation }

case class Replay(setup: Game, moves: List[MoveOrDrop], state: Game) {

  lazy val chronoMoves = moves.reverse

  def addMove(moveOrDrop: MoveOrDrop) =
    copy(
      moves = moveOrDrop.left.map(_.applyVariantEffect) :: moves,
      state = moveOrDrop.fold(state.apply, state.applyDrop)
    )

  def moveAtPly(ply: Int): Option[MoveOrDrop] =
    chronoMoves lift (ply - 1 - setup.startedAtTurn)
}

object Replay {

  def apply(game: Game) = new Replay(game, Nil, game)

  def apply(
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, Reader.Result] =
    moveStrs.some.filter(_.nonEmpty) toValid "[replay] pgn is empty" andThen { nonEmptyMoves =>
      Reader.moves(
        nonEmptyMoves,
        Tags(
          List(
            initialFen map { fen =>
              Tag(_.FEN, fen.value)
            },
            variant.some.filterNot(_.shogi) map { v =>
              Tag(_.Variant, v.name)
            }
          ).flatten
        )
      )
    }

  //private def recursiveGames(game: Game, sans: List[San]): Validated[String, List[Game]] =
  //  sans match {
  //    case Nil => valid(Nil)
  //    case san :: rest =>
  //      san(StratSituation.wrap(game.situation)) flatMap { moveOrDrop =>
  //        val newGame = StratGame.wrap(game)(moveOrDrop).toFairySF
  //        recursiveGames(newGame, rest) map { newGame :: _ }
  //      }
  //  }

  //def games(
  //    moveStrs: Iterable[String],
  //    initialFen: Option[FEN],
  //    variant: strategygames.fairysf.variant.Variant
  //): Validated[String, List[Game]] =
  //  Parser.moves(moveStrs, variant) andThen { moves =>
  //    val game = makeGame(variant, initialFen)
  //    recursiveGames(game, moves.value) map { game :: _ }
  //  }

  def gameMoveWhileValid(
      moveStrs: Seq[String],
      initialFen: FEN,
      variant: strategygames.fairysf.variant.Variant
  ): (Game, List[(Game, Uci.WithSan)], Option[String]) = {

    val init = makeGame(variant, initialFen.some)
    var state = init
    var uciMoves = init.situation.board.uciMoves
    var errors = ""
    val moves: List[(Game, Uci.WithSan)] = moveStrs.toList
      .map{
        case Uci.Move.moveR(orig, dest, check) => (Pos.fromKey(orig), Pos.fromKey(dest), check)
      }.map{
        case (Some(orig), Some(dest), check) => {
          val uciMove = s"${orig.key}${dest.key}${check}"
          uciMoves = uciMoves :+ uciMove
          val fen = Api.fenFromMoves(
            init.board.variant.fairysfName.name,
            init.situation.board.variant.initialFen.value,
            uciMoves.some
          ).value
          state = state.apply(
            Move(
              piece = state.situation.board.pieces(orig),
              orig = orig,
              dest = dest,
              situationBefore = state.situation,
              after = state.situation.board.copy(
                pieces = Api.pieceMapFromFen(
                  init.board.variant.fairysfName.name,
                  fen
                ),
                uciMoves = uciMoves,
                pocketData = Api.pocketData(init.board.variant, fen)
              ),
              capture = None,
              promotion = None,
              castle = None,
              enpassant = false
            )
          )
          (state, Uci.WithSan(Uci.apply(state.board.variant.gameFamily, uciMove).get, "NOSAN"))
        }
        case (orig, dest, check) => {
          val uciMove = s"${orig}${dest}${check}"
          errors += uciMove + ","
          sys.error(s"Invalid move: ${uciMove}")
        }
      }
    (init, moves, errors match {case "" => None; case _ => errors.some})
  }

  private def recursiveSituations(sit: Situation, sans: List[San]): Validated[String, List[Situation]] =
    sans match {
      case Nil => valid(Nil)
      case san :: rest =>
        san(StratSituation.wrap(sit)) flatMap { moveOrDrop =>
          val after = Situation(moveOrDrop.fold(m => m.finalizeAfter().toFairySF, d => d.finalizeAfter.toFairySF), !sit.color)
          recursiveSituations(after, rest) map { after :: _ }
        }
    }

  private def recursiveSituationsFromUci(
      sit: Situation,
      ucis: List[Uci]
  ): Validated[String, List[Situation]] =
    ucis match {
      case Nil => valid(Nil)
      case uci :: rest =>
        uci(sit) andThen { moveOrDrop =>
          val after = Situation(moveOrDrop.fold(_.finalizeAfter, _.finalizeAfter), !sit.color)
          recursiveSituationsFromUci(after, rest) map { after :: _ }
        }
    }

  private def recursiveReplayFromUci(replay: Replay, ucis: List[Uci]): Validated[String, Replay] =
    ucis match {
      case Nil => valid(replay)
      case uci :: rest =>
        uci(replay.state.situation) andThen { moveOrDrop =>
          recursiveReplayFromUci(replay addMove moveOrDrop, rest)
        }
    }

  private def initialFenToSituation(
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Situation = {
    initialFen.flatMap(Forsyth.<<) | Situation(strategygames.fairysf.variant.Variant.default)
  } withVariant variant

  def boards(
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, List[Board]] = situations(moveStrs, initialFen, variant) map (_ map (_.board))

  def situations(
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    Parser.moves(moveStrs, sit.board.variant) andThen { moves =>
      recursiveSituations(sit, moves.value) map { sit :: _ }
    }
  }

  def boardsFromUci(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, List[Board]] = situationsFromUci(moves, initialFen, variant) map (_ map (_.board))

  def situationsFromUci(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, List[Situation]] = {
    val sit = initialFenToSituation(initialFen, variant)
    recursiveSituationsFromUci(sit, moves) map { sit :: _ }
  }

  def apply(
      moves: List[Uci],
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant
  ): Validated[String, Replay] =
    recursiveReplayFromUci(Replay(makeGame(variant, initialFen)), moves)

  def plyAtFen(
      moveStrs: Iterable[String],
      initialFen: Option[FEN],
      variant: strategygames.fairysf.variant.Variant,
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
          case Nil => invalid(s"Can't find $atFenTruncated, reached ply $ply")
          case san :: rest =>
            san(StratSituation.wrap(sit)) flatMap { moveOrDrop =>
              val after = moveOrDrop.fold(m => m.finalizeAfter().toFairySF, d => d.finalizeAfter.toFairySF)
              val fen   = Forsyth >> Game(Situation(after, Color.fromPly(ply)), turns = ply)
              if (compareFen(fen)) Validated.valid(ply)
              else recursivePlyAtFen(Situation(after, !sit.color), rest, ply + 1)
            }
        }

      val sit = initialFen.flatMap {
        Forsyth.<<@(variant, _)
      } | Situation(variant)

      Parser.moves(moveStrs, sit.board.variant) andThen { moves =>
        recursivePlyAtFen(sit, moves.value, 1)
      }
    }

  private def makeGame(variant: strategygames.fairysf.variant.Variant, initialFen: Option[FEN]): Game = {
    val g = Game(variant.some, initialFen)
    g.copy(startedAtTurn = g.turns)
  }
}
