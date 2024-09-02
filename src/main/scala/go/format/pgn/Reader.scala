package strategygames.go
package format.pgn
import strategygames.{ Action => StratAction, ActionStrs, ByoyomiClock, Clock, Situation => StratSituation }

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

  def fullWithSans(pgn: String, op: Sans => Sans, tags: Tags = Tags.empty): Validated[String, Result] =
    Parser.full(cleanUserInput(pgn)) map { parsed =>
      makeReplay(makeGame(parsed.tags ++ tags), op(parsed.sans))
    }

  def fullWithSans(parsed: ParsedPgn, op: Sans => Sans): Result =
    makeReplay(makeGame(parsed.tags), op(parsed.sans))

  def replayResultFromActionStrs(
      actionStrs: ActionStrs,
      op: ActionStrs => ActionStrs,
      tags: Tags
  ): Validated[String, Result] =
    Validated.valid(makeReplayWithActionStrs(makeGame(tags), op(actionStrs)))

  // remove invisible byte order mark
  def cleanUserInput(str: String) = str.replace(s"\ufeff", "")

  private def makeReplay(game: Game, sans: Sans): Result =
    sans.value.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), san) =>
        san(StratSituation.wrap(replay.state.situation)).fold(
          err => Result.Incomplete(replay, err),
          action => Result.Complete(replay addAction StratAction.toGo(action))
        )
      case (r: Result.Incomplete, _)      => r
    }

  private def makeReplayWithActionStrs(game: Game, actionStrs: ActionStrs): Result =
    Replay.actionStrsWithEndTurn(actionStrs).foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), (actionStr, endTurn)) =>
        actionStr match {
          case Uci.Drop.dropR(role, dest)           =>
            (Role.allByForsyth(replay.state.board.variant.gameFamily).get(role(0)), Pos.fromKey(dest)) match {
              case (Some(role), Some(dest)) =>
                Result.Complete(
                  replay.addAction(
                    Replay.replayDrop(
                      replay.state,
                      role,
                      dest,
                      endTurn
                    )
                  )
                )
              case _                        => Result.Incomplete(replay, s"Error making replay with drop: ${actionStr}")
            }
          case Uci.Pass.passR()                     =>
            Result.Complete(
              replay.addAction(
                Replay.replayPass(
                  replay.state,
                  endTurn,
                  replay.state.board.apiPosition.makeMoves(List(actionStr)),
                  replay.state.board.uciMoves :+ actionStr
                )
              )
            )
          case Uci.SelectSquares.selectSquaresR(ss) =>
            Result.Complete(
              replay.addAction(
                Replay.replaySelectSquares(
                  replay.state,
                  ss.split(",").toList.flatMap(Pos.fromKey(_)),
                  endTurn,
                  replay.state.board.apiPosition.makeMoves(List(actionStr)),
                  replay.state.board.uciMoves :+ actionStr
                )
              )
            )
          case _                                    => Result.Incomplete(replay, s"Error making replay with uci: ${actionStr}")
        }
      case (r: Result.Incomplete, _)                       => r
    }

  private def makeGame(tags: Tags) = {
    val g = Game(
      variantOption = tags.goVariant,
      fen = tags.goFen
    )
    g.copy(
      startedAtPly = g.plies,
      startedAtTurn = g.turnCount,
      clock = tags.clockConfig.flatMap {
        case fc: Clock.Config        => Some(Clock.apply(fc))
        case bc: ByoyomiClock.Config => Some(ByoyomiClock.apply(bc))
        case _                       => None
      }
    )
  }
}
