package strategygames.dameo
package format.pdn
import strategygames.{
  Action => StratAction,
  ActionStrs,
  ByoyomiClock,
  Clock,
  Move => StratMove,
  Situation => StratSituation
}
import strategygames.format.pgn.{ ParsedPgn => ParsedPdn, Sans, Tags }

import cats.data.Validated
import scala.annotation.nowarn

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

  def full(pdn: String, tags: Tags = Tags.empty): Validated[String, Result] =
    fullWithSans(pdn, identity, tags, true)

  def replayResult(
      actionStrs: ActionStrs,
      tags: Tags,
      iteratedCapts: Boolean = false
  ): Validated[String, Result] =
    replayResultFromActionStrsUsingSan(actionStrs, identity, tags, iteratedCapts)

  def fullWithSans(
      pdn: String,
      op: Sans => Sans,
      tags: Tags = Tags.empty,
      iteratedCapts: Boolean = false
  ): Validated[String, Result] =
    Parser.full(cleanUserInput(pdn)) map { parsed =>
      makeReplay(makeGame(parsed.tags ++ tags), op(parsed.sans), iteratedCapts)
    }

  def fullWithSans(parsed: ParsedPdn, op: Sans => Sans): Result =
    makeReplay(makeGame(parsed.tags), op(parsed.sans))

  def replayResultFromActionStrsUsingSan(
      actionStrs: ActionStrs,
      op: Sans => Sans,
      tags: Tags,
      @nowarn iteratedCapts: Boolean = false
  ): Validated[String, Result] =
    // Its ok to flatten actionStrs as the game is built back up again from the Situation
    Parser.sans(actionStrs.flatten, tags.dameoVariant | variant.Variant.default) map { sans =>
      makeReplay(makeGame(tags), op(sans))
    }

  // remove invisible byte order mark
  def cleanUserInput(str: String) = str.replace(s"\ufeff", "")

  // TODO: because this is primarily used in a Validation context, we should be able to
  //       return something that's runtime safe as well.
  def dameoMove(action: StratAction) = action match {
    case StratMove.Dameo(m) => m
    case _                  => sys.error("Invalid dameo move")
  }

  private def makeReplay(game: Game, sans: Sans, @nowarn iteratedCapts: Boolean = false): Result =
    sans.value.foldLeft[Result](Result.Complete(Replay(game))) {
      case (Result.Complete(replay), san) =>
        san(StratSituation.wrap(replay.state.situation)).fold(
          err => Result.Incomplete(replay, err),
          action => Result.Complete(replay addAction StratAction.toDameo(action))
        )
      case (r: Result.Incomplete, _)      => r
    }

  private def makeGame(tags: Tags) = {
    val g = Game(
      variantOption = tags.dameoVariant,
      fen = tags.dameoFen
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
