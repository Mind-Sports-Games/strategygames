package strategygames.go
package format.pgn
import strategygames.{
  Action => StratAction,
  Actions,
  ByoyomiClock,
  FischerClock,
  Situation => StratSituation
}

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

  def replayResultFromActions(
      actions: Actions,
      op: Actions => Actions,
      tags: Tags
  ): Validated[String, Result] =
    Validated.valid(makeReplayWithActions(makeGame(tags), op(actions)))

  // remove invisible byte order mark
  def cleanUserInput(str: String) = str.replace(s"\ufeff", "")

  private def makeReplay(game: Game, sans: Sans): Result =
    sans.value.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), san) =>
        san(StratSituation.wrap(replay.state.situation)).fold(
          err => Result.Incomplete(replay, err),
          action => Result.Complete(replay addPly StratAction.toGo(action))
        )
      case (r: Result.Incomplete, _)      => r
    }

  private def makeReplayWithActions(game: Game, actions: Actions): Result =
    Replay.pliesWithEndTurn(actions).foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), (action, endTurn)) =>
        action match {
          case Uci.Drop.dropR(role, dest)           =>
            (Role.allByForsyth(replay.state.board.variant.gameFamily).get(role(0)), Pos.fromKey(dest)) match {
              case (Some(role), Some(dest)) =>
                Result.Complete(
                  replay.addPly(
                    Replay.replayDrop(
                      replay.state,
                      role,
                      dest,
                      endTurn,
                      replay.state.board.apiPosition.makeMoves(List(action)),
                      replay.state.board.uciMoves :+ action
                    )
                  )
                )
              case _                        => Result.Incomplete(replay, s"Error making replay with drop: ${action}")
            }
          case Uci.Pass.passR()                     =>
            Result.Complete(
              replay.addPly(
                Replay.replayPass(
                  replay.state,
                  endTurn,
                  replay.state.board.apiPosition.makeMoves(List(action)),
                  replay.state.board.uciMoves :+ action
                )
              )
            )
          case Uci.SelectSquares.selectSquaresR(ss) =>
            Result.Complete(
              replay.addPly(
                Replay.replaySelectSquares(
                  replay.state,
                  ss.split(",").toList.flatMap(Pos.fromKey(_)),
                  endTurn,
                  replay.state.board.apiPosition.makeMoves(List(action)),
                  replay.state.board.uciMoves :+ action
                )
              )
            )
          case _                                    => Result.Incomplete(replay, s"Error making replay with uci: ${action}")
        }
      case (r: Result.Incomplete, _)                    => r
    }

  private def makeGame(tags: Tags) = {
    val g = Game(
      variantOption = tags.goVariant,
      fen = tags.goFen
    )
    g.copy(
      startedAtPlies = g.plies,
      startedAtTurn = g.turnCount,
      clock = tags.clockConfig.flatMap {
        case fc: FischerClock.Config => Some(FischerClock.apply(fc))
        case bc: ByoyomiClock.Config => Some(ByoyomiClock.apply(bc))
        case _                       => None
      }
    )
  }
}
