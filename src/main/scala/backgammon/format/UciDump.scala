package strategygames.backgammon.format

import scala.annotation.nowarn
import cats.data.Validated

import strategygames.backgammon.variant.Variant
import strategygames.backgammon.{ Action, Move, Replay }
import strategygames.{ ActionStrs, Player }

object UciDump {

  // a2a4, b8c6
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
        initialFen = initialFen,
        variant = variant
      ) andThen (_.valid) map apply

  def action(@nowarn variant: Variant)(a: Action): String = a.toUci.uci

}
