package strategygames.dameo
package format

import scala.annotation.nowarn

import cats.data.Validated

import strategygames.dameo.variant.Variant
import strategygames.ActionStrs

object UciDump {

  def apply(replay: Replay): ActionStrs =
    replay.chronoActions.map(_.map(action(replay.setup.board.variant)))

  def apply(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: Variant
  ): Validated[String, ActionStrs] =
    if (actionStrs.isEmpty) Validated.valid(Nil)
    else Replay(actionStrs, initialFen, variant) andThen (_.valid) map apply

  def action(@nowarn _variant: Variant)(action: Action): String = action match {
    case m: Move => m.toUci.shortUci
  }

}
