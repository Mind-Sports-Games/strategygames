package strategygames.draughts
package format

import cats.data.Validated

import strategygames.draughts.variant.Variant
import strategygames.ActionStrs

object UciDump {

  def apply(replay: Replay): ActionStrs =
    replay.chronoActions.map(_.map(action(replay.setup.board.variant)))

  def apply(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, ActionStrs] =
    if (actionStrs.isEmpty) Validated.valid(Nil)
    else Replay(actionStrs, initialFen, variant, finalSquare) andThen (_.valid) map apply

  def action(_variant: Variant)(action: Action): String = action match {
    case m: Move => m.toUci.shortUci
  }

}
