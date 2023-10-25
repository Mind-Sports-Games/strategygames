package strategygames.chess.format

import cats.data.Validated

import strategygames.chess.variant.Variant
import strategygames.chess.{ Action, Drop, Move, Replay }
import strategygames.ActionStrs

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
    else Replay(actionStrs, initialFen, variant) andThen (_.valid) map apply

  def action(variant: Variant)(action: Action): String =
    action match {
      case m: Move =>
        m.castle.fold(m.toUci.uci) {
          case ((kf, kt), (rf, _)) if kf == kt || variant.chess960 || variant.fromPosition => kf.key + rf.key
          case ((kf, kt), _)                                                               => kf.key + kt.key
        }
      case d: Drop => d.toUci.uci
    }
}
