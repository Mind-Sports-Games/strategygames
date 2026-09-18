package strategygames.entropy
package format.pgn

import strategygames.ActionStrs
import strategygames.format.pgn.Tags

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

  // entropy records games as action strings; there is no PGN text parser for it
  def replayResultFromActionStrs(
      actionStrs: ActionStrs,
      op: ActionStrs => ActionStrs,
      @annotation.nowarn tags: Tags
  ): Validated[String, Result] =
    Replay(op(actionStrs), None, strategygames.entropy.variant.Variant.default)

}
