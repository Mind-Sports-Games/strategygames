package strategygames.entropy.format

import scala.annotation.nowarn
import cats.data.Validated

import strategygames.entropy.variant.Variant
import strategygames.entropy.{ Action, Replay }
import strategygames.{ ActionStrs, Player }

object UciDump {

  def apply(replay: Replay): ActionStrs =
    replay.chronoActions.map(_.map(action(replay.setup.board.variant)))

  def apply(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: Variant
  ): Validated[String, ActionStrs] =
    if (actionStrs.isEmpty) Validated.valid(Nil)
    else
      Replay(
        actionStrs = actionStrs,
        startPlayer = Player.P1,
        activePlayer = Player.fromTurnCount(actionStrs.size),
        initialFen = initialFen,
        variant = variant
      ) andThen (_.valid) map apply

  def action(@nowarn variant: Variant)(a: Action): String = a.toUci.uci

}
