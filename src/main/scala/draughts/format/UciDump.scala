package strategygames.draughts
package format

import scala.annotation.nowarn

import cats.data.Validated

import strategygames.draughts.variant.Variant

object UciDump {

  def apply(replay: Replay): List[String] =
    replay.chronoMoves map move(replay.setup.board.variant)

  def apply(
      moves: Seq[String],
      initialFen: Option[FEN],
      variant: Variant,
      finalSquare: Boolean = false
  ): Validated[String, List[String]] =
    if (moves.isEmpty)
      Validated.valid(Nil)
    else
      Replay(moves, initialFen, variant, finalSquare) andThen (_.valid) map apply

  def move(@nowarn _variant: Variant)(action: Action): String = action match {
    case m: Move => m.toUci.shortUci
  }

}
