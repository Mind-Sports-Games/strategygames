package strategygames.fiveinarow
package format.pgn

import strategygames.ActionStrs
import strategygames.format.pgn.Tags

import cats.data.Validated
import scalalib.extensions.*

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

  // the variant comes from the tags: a fen cannot always tell gomoku from renju, both played on 15x15
  def replayResultFromActionStrs(
      actionStrs: ActionStrs,
      op: ActionStrs => ActionStrs,
      tags: Tags
  ): Validated[String, Result] =
    Replay(
      op(actionStrs),
      tags.fiveInARowFen,
      tags.fiveInARowVariant | strategygames.fiveinarow.variant.Variant.default
    )

}
