package strategygames.draughts
package format

import cats.data.Validated

import strategygames.draughts.variant.Variant
import strategygames.Actions

object UciDump {

  def apply(replay: Replay): Actions =
    replay.chronoActions.map(_.map(action(replay.setup.board.variant)))

  def apply(
      actions: Actions,
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, Actions] =
    if (actions.isEmpty) Validated.valid(Nil)
    else Replay(actions, initialFen, variant, finalSquare) andThen (_.valid) map apply

  def action(_variant: Variant)(mod: Move): String = mod.toUci.shortUci

}
