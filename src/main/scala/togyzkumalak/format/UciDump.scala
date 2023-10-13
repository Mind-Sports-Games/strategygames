package strategygames.togyzkumalak.format

import cats.data.Validated

import strategygames.togyzkumalak.variant.Variant
import strategygames.togyzkumalak.{ Move, Replay }
import strategygames.{ Actions, Player }

object UciDump {

  // a2a4, b8c6
  def apply(replay: Replay): Actions =
    replay.chronoActions.map(_.map(action(replay.setup.board.variant)))

  def apply(
      actions: Actions,
      initialFen: Option[FEN],
      variant: Variant
  ): Validated[String, Actions] =
    if (actions.isEmpty) Validated.valid(Nil)
    else
      Replay(
        actions = actions,
        // we can default to this because in UciDump we are only looking to validate
        // the current actions, not work out future actions
        startPlayer = Player.P1,
        activePlayer = Player.fromTurnCount(actions.size),
        initialFen = initialFen,
        variant = variant
      ) andThen (_.valid) map apply

  def action(variant: Variant)(mod: Move): String = mod.toUci.uci

}
