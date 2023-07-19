package strategygames.go.format

import cats.data.Validated

import strategygames.go.variant.Variant
import strategygames.go.{ Action, Drop, Pass, Replay }

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

  def move(variant: Variant)(action: Action): String = action match {
    case p: Pass => p.toUci.uci
    case d: Drop => d.toUci.uci
  }

}
