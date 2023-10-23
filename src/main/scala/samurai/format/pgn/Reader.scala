package strategygames.samurai
package format.pgn
import strategygames.{ Actions, ByoyomiClock, FischerClock, Move => StratMove, Situation => StratSituation }

import strategygames.format.pgn.{ ParsedPgn, Sans, Tags }

import strategygames.samurai.format.Uci

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
          ply => Result.Complete(replay addPly StratMove.toSamurai(ply))
        )
      case (r: Result.Incomplete, _)      => r
    }

  private def makeReplayWithActions(game: Game, actions: Actions): Result =
    Replay.pliesWithEndTurn(actions).foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), (action, endTurn)) =>
        action match {
          case Uci.Move.moveR(orig, dest, _) => {
            (Pos.fromKey(orig), Pos.fromKey(dest)) match {
              case (Some(orig), Some(dest)) =>
                Result.Complete(
                  replay.addPly(
                    Replay.replayMove(
                      replay.state,
                      orig,
                      dest,
                      endTurn,
                      replay.state.board.apiPosition
                        .makeMoves(List(action).map(uciMove => Api.uciToMove(uciMove))),
                      replay.state.board.uciMoves :+ action
                    )
                  )
                )
              case _                        =>
                Result.Incomplete(replay, s"Error making replay with move: ${action}")
            }
          }
          case _                             =>
            Result.Incomplete(replay, s"Error making replay with uci move: ${action}")
        }
      case (r: Result.Incomplete, _)                    => r
    }

  private def makeGame(tags: Tags) = {
    val g = Game(
      variantOption = tags.samuraiVariant,
      fen = tags.samuraiFen
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
