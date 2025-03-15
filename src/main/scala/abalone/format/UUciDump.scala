package strategygames.abalone.format

import cats.data.Validated
import strategygames.abalone.{AAction, MMove, RReplay}
import strategygames.abalone.variant.Variant
import strategygames.{ActionStrs, Player}

import scala.annotation.nowarn

object UUciDump {
  // a2a4, b8c6
  def apply(replay: RReplay): ActionStrs =
    replay.chronoActions.map(_.map(action(replay.setup.board.variant)))

  def apply(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: Variant
  ): Validated[String, ActionStrs] =
    if (actionStrs.isEmpty) Validated.valid(Nil)
    else
      RReplay(
        actionStrs = actionStrs,
        // we can default to this because in UciDump we are only looking to validate
        // the current actionStrs, not work out future actionStrs
        startPlayer = Player.P1,
        activePlayer = Player.fromTurnCount(actionStrs.size),
        initialFen = initialFen,
        variant = variant
      ) andThen (_.valid) map apply

  def action(@nowarn variant: Variant)(a: AAction): String = a match {
    case m: MMove => m.toUci.uci
  }
}