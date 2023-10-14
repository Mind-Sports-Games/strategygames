package strategygames.go
package format.pgn
import strategygames.{ ByoyomiClock, Clock, Situation => StratSituation }

import strategygames.format.pgn.{ ParsedPgn, Sans, Tags }

import strategygames.go.format.Uci

import cats.data.Validated
import cats.implicits._

object Reader {

  sealed trait Result {
    def valid: Validated[String, Replay]
  }

  object Result {
    case class Complete(replay: Replay)                    extends Result {
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

  def fullWithSans(pgn: String, op: Sans => Sans, tags: Tags = Tags.empty): Validated[String, Result] =
    Parser.full(cleanUserInput(pgn)) map { parsed =>
      makeReplay(makeGame(parsed.tags ++ tags), op(parsed.sans))
    }

  def fullWithSans(parsed: ParsedPgn, op: Sans => Sans): Result =
    makeReplay(makeGame(parsed.tags), op(parsed.sans))

  def movesWithSans(moveStrs: Iterable[String], op: Sans => Sans, tags: Tags): Validated[String, Result] =
    Parser.moves(moveStrs, tags.goVariant | variant.Variant.default) map { moves =>
      makeReplay(makeGame(tags), op(moves))
    }

  def movesWithPgns(
      moveStrs: Iterable[String],
      op: Iterable[String] => Iterable[String],
      tags: Tags
  ): Validated[String, Result] =
    Validated.valid(makeReplayWithPgn(makeGame(tags), op(moveStrs)))

  // remove invisible byte order mark
  def cleanUserInput(str: String) = str.replace(s"\ufeff", "")

  private def makeReplay(game: Game, sans: Sans): Result =
    sans.value.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), san) =>
        san(StratSituation.wrap(replay.state.situation)).fold(
          err => Result.Incomplete(replay, err),
          action => Result.Complete(replay addMove action.toGo)
        )
      case (r: Result.Incomplete, _)      => r
    }

  private def makeReplayWithPgn(game: Game, moves: Iterable[String]): Result =
    Parser.pgnMovesToUciMoves(moves).foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), m) =>
        m match {
          case Uci.Drop.dropR(role, dest)           =>
            (Role.allByForsyth(replay.state.board.variant.gameFamily).get(role(0)), Pos.fromKey(dest)) match {
              case (Some(role), Some(dest)) =>
                Result.Complete(
                  replay.addMove(
                    Replay.replayDrop(
                      replay.state,
                      role,
                      dest,
                      replay.state.board.apiPosition.makeMoves(List(m)),
                      replay.state.board.uciMoves :+ m
                    )
                  )
                )
              case _                        => Result.Incomplete(replay, s"Error making replay with drop: ${m}")
            }
          case Uci.Pass.passR()                     =>
            Result.Complete(
              replay.addMove(
                Replay.replayPass(
                  replay.state,
                  replay.state.board.apiPosition.makeMoves(List(m)),
                  replay.state.board.uciMoves :+ m
                )
              )
            )
          case Uci.SelectSquares.selectSquaresR(ss) =>
            Result.Complete(
              replay.addMove(
                Replay.replaySelectSquares(
                  replay.state,
                  ss.split(",").toList.flatMap(Pos.fromKey(_)),
                  replay.state.board.apiPosition.makeMoves(List(m)),
                  replay.state.board.uciMoves :+ m
                )
              )
            )
          case _                                    => Result.Incomplete(replay, s"Error making replay with uci move: ${m}")
        }
      case (r: Result.Incomplete, _)    => r
    }

  private def makeGame(tags: Tags) = {
    val g = Game(
      variantOption = tags.goVariant,
      fen = tags.goFen
    )
    g.copy(
      startedAtTurn = g.turns,
      clock = tags.clockConfig.flatMap {
        case fc: Clock.Config => Some(Clock.apply(fc))
        case bc: ByoyomiClock.Config => Some(ByoyomiClock.apply(bc))
        case _                       => None
      }
    )
  }
}
