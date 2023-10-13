package strategygames.chess
package format.pgn
import strategygames.{ Actions, ByoyomiClock, FischerClock, Move => StratMove, Situation => StratSituation }

import strategygames.format.pgn.{ ParsedPgn, Sans, Tags }

import cats.data.Validated

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

  def replayResult(actions: Actions, tags: Tags): Validated[String, Result] =
    replayResultFromActionsUsingSan(actions, identity, tags)

  def fullWithSans(pgn: String, op: Sans => Sans, tags: Tags = Tags.empty): Validated[String, Result] =
    Parser.full(cleanUserInput(pgn)) map { parsed =>
      makeReplay(makeGame(parsed.tags ++ tags), op(parsed.sans))
    }

  def fullWithSans(parsed: ParsedPgn, op: Sans => Sans): Result =
    makeReplay(makeGame(parsed.tags), op(parsed.sans))

  def replayResultFromActionsUsingSan(
      actions: Actions,
      op: Sans => Sans,
      tags: Tags
  ): Validated[String, Result] =
    // Its ok to flatten actions as the game is built back up again from the Situation
    Parser.sans(actions.flatten, tags.chessVariant | variant.Variant.default) map { sans =>
      makeReplay(makeGame(tags), op(sans))
    }

  // remove invisible byte order mark
  def cleanUserInput(str: String) = str.replace(s"\ufeff", "")

  private def makeReplay(game: Game, sans: Sans): Result =
    sans.value.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), san) =>
        san(StratSituation.wrap(replay.state.situation)).fold(
          err => Result.Incomplete(replay, err),
          // TODO probably want this extended to cover Drops and not just Moves
          move => Result.Complete(replay addPly StratMove.toChess(move))
        )
      case (r: Result.Incomplete, _)      => r
    }

  private def makeGame(tags: Tags) = {
    val g = Game(
      variantOption = tags.chessVariant,
      fen = tags.chessFen
    )
    g.copy(
      startedAtTurn = g.currentTurnCount,
      startPlayer = g.situation.player,
      clock = tags.clockConfig.flatMap {
        case fc: FischerClock.Config => Some(FischerClock.apply(fc))
        case bc: ByoyomiClock.Config => Some(ByoyomiClock.apply(bc))
        case _                       => None
      }
    )
  }
}
