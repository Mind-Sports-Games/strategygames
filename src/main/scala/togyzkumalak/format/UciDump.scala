package strategygames.togyzkumalak.format

import cats.data.Validated

import strategygames.togyzkumalak.variant.Variant
import strategygames.togyzkumalak.{ Move, Replay }

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

  def move(variant: Variant)(mod: Move): String = mod.toUci.uci

}
