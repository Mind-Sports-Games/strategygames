package draughts
package format

import cats.data.Validated

import draughts.variant.Variant

object UciDump {

  def apply(replay: Replay): List[String] =
    replay.chronoMoves map move(replay.setup.board.variant)

  def apply(moves: Seq[String], initialFen: Option[String], variant: Variant, finalSquare: Boolean = false): Validated[String, List[String]] =
    moves.isEmpty.fold(
      Validated.valid(Nil),
      Replay(moves, initialFen, variant, finalSquare) andThen (_.valid) map apply
    )

  def move(variant: Variant)(mod: Move): String = mod.toUci.uci

}
