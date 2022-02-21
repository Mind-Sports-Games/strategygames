package strategygames.fairysf
package format.pgn
import strategygames.{ Clock, GameFamily, Move => StratMove, Situation => StratSituation }

import strategygames.format.pgn.{ ParsedPgn, Sans, Tags }

import strategygames.fairysf.format.Uci

import cats.data.Validated

object Reader {

  sealed trait Result {
    def valid: Validated[String, Replay]
  }

  object Result {
    case class Complete(replay: Replay) extends Result {
      def valid = Validated.valid(replay)
    }
    case class Incomplete(replay: Replay, failure: String) extends Result {
      def valid = Validated.invalid(failure)
    }
  }

  def full(pgn: String, tags: Tags = Tags.empty): Validated[String, Result] =
    fullWithSans(pgn, identity, tags)

  def moves(moveStrs: Iterable[String], tags: Tags): Validated[String, Result] =
    movesWithSans(moveStrs, identity, tags)

  // Because fairysf deals exclusively with UCI moves, we need this version
  // for parsing replaces from uci move
  def uciMoves(gf: GameFamily, moveStrs: Iterable[String], tags: Tags): Validated[String, Result] = {
    val moves = moveStrs.flatMap(Uci(gf, _))
    if (moves.size < moveStrs.size) {
      Validated.invalid("Invalid UCI moves")
    } else {
      Validated.valid(
        makeReplayFromUCI(
          makeGame(tags),
          moves.toList
        )
      )
    }
  }

  def fullWithSans(pgn: String, op: Sans => Sans, tags: Tags = Tags.empty): Validated[String, Result] =
    Parser.full(cleanUserInput(pgn)) map { parsed =>
      makeReplay(makeGame(parsed.tags ++ tags), op(parsed.sans))
    }

  def fullWithSans(parsed: ParsedPgn, op: Sans => Sans): Result =
    makeReplay(makeGame(parsed.tags), op(parsed.sans))

  def movesWithSans(moveStrs: Iterable[String], op: Sans => Sans, tags: Tags): Validated[String, Result] =
    Parser.moves(moveStrs, tags.fairysfVariant | variant.Variant.default) map { moves =>
      makeReplay(makeGame(tags), op(moves))
    }

  def movesWithPgns(moveStrs: Iterable[String], op: Iterable[String] => Iterable[String], tags: Tags): Validated[String, Result] =
    Validated.valid(makeReplayWithPgn(makeGame(tags), op(moveStrs)))

  // remove invisible byte order mark
  def cleanUserInput(str: String) = str.replace(s"\ufeff", "")

  private def makeReplay(game: Game, sans: Sans): Result =
    sans.value.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), san) =>
        san(StratSituation.wrap(replay.state.situation)).fold(
          err => Result.Incomplete(replay, err),
          move => Result.Complete(replay addMove StratMove.toFairySF(move))
        )
      case (r: Result.Incomplete, _) => r
    }

  private def makeReplayFromUCI(game: Game, ucis: List[Uci]): Result =
    ucis.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), uci) =>
        uci(replay.state.situation).fold(
          err => Result.Incomplete(replay, err),
          move => Result.Complete(replay addMove move)
        )
      case (r: Result.Incomplete, _) => r
    }

  private def makeReplayWithPgn(game: Game, moves: Iterable[String]): Result =
    Parser.pgnMovesToUciMoves(moves).foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), m) =>
        m match {
          case Uci.Move.moveR(orig, dest, promotion) => {
            (Pos.fromKey(orig), Pos.fromKey(dest)) match {
              case (Some(orig), Some(dest)) => Result.Complete(
                replay.addMove(
                  Left(Replay.replayMove(
                    replay.state,
                    orig,
                    dest,
                    promotion,
                    replay.state.board.apiPosition.makeMoves(List(m)),
                    replay.state.board.uciMoves :+ m
                  ))
                )
              )
              case _ => Result.Incomplete(replay, s"Error making replay with move: ${m}")
            }
          }
          case Uci.Drop.dropR(role, dest) =>
            (Role.allByForsyth(replay.state.board.variant.gameFamily).get(role(0)), Pos.fromKey(dest)) match {
              case (Some(role), Some(dest)) => Result.Complete(
                replay.addMove(
                  Right(Replay.replayDrop(
                    replay.state,
                    role,
                    dest,
                    replay.state.board.apiPosition.makeMoves(List(m)),
                    replay.state.board.uciMoves :+ m
                  ))
                )
              )
              case _ => Result.Incomplete(replay, s"Error making replay with drop: ${m}")
            }
          case _ => Result.Incomplete(replay, s"Error making replay with uci move: ${m}")
        }
      case (r: Result.Incomplete, _) => r
    }

  private def makeGame(tags: Tags) = {
    val g = Game(
      variantOption = tags(_.Variant) flatMap strategygames.fairysf.variant.Variant.byName,
      fen = tags.fairysfFen
    )
    g.copy(
      startedAtTurn = g.turns,
      clock = tags.clockConfig map (config => Clock.apply(config))
    )
  }
}
