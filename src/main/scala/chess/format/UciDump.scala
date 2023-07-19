package strategygames.chess.format

import cats.data.Validated

import strategygames.chess.variant.Variant
import strategygames.chess.{ Action, Drop, Move, Replay }

object UciDump {

  // a2a4, b8c6
  def apply(replay: Replay): List[String] =
    replay.chronoMoves map move(replay.setup.board.variant)

  def apply(
      moves: Seq[String],
      initialFen: Option[FEN],
      variant: Variant
  ): Validated[String, List[String]] =
    if (moves.isEmpty) Validated.valid(Nil)
    else Replay(moves, initialFen, variant) andThen (_.valid) map apply

  def move(variant: Variant)(action: Action): String =
    action match {
      case m: Move =>
        m.castle.fold(m.toUci.uci) {
          case ((kf, kt), (rf, _)) if kf == kt || variant.chess960 || variant.fromPosition => kf.key + rf.key
          case ((kf, kt), _)                                                               => kf.key + kt.key
        }
      case d: Drop => d.toUci.uci
    }
}
