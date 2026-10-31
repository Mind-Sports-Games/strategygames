package strategygames.yonkoma
package format.pgn

import strategygames.ActionStrs
import strategygames.format.pgn.Tags
import strategygames.yonkoma.format.FEN
import strategygames.yonkoma.variant.Variant

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

  // yonkoma records games as action strings; there is no PGN text parser for it. The
  // board size comes from the Variant tag, so a 12x12 game replays on a 12x12 board.
  def replayResultFromActionStrs(
      actionStrs: ActionStrs,
      op: ActionStrs => ActionStrs,
      tags: Tags
  ): Validated[String, Result] =
    Replay(op(actionStrs), tags(_.FEN).map(FEN.apply), variantFromTags(tags))

  private def variantFromTags(tags: Tags): Variant =
    tags(_.Variant)
      .flatMap(name => Variant.byName(name) orElse Variant(name.toLowerCase))
      .getOrElse(Variant.default)

}
