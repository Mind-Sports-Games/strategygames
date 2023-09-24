package strategygames.go.format

import cats.data.Validated

import strategygames.go.variant.Variant
import strategygames.go.{ Action, Drop, Pass, Replay, SelectSquares }

object UciDump {

  // a2a4, b8c6
  def apply(replay: Replay): List[String] =
    replay.chronoMoves map move

  def apply(
      moves: Seq[String],
      initialFen: Option[FEN],
      variant: Variant
  ): Validated[String, List[String]] =
    if (moves.isEmpty) Validated.valid(Nil)
    else Replay(moves, initialFen, variant) andThen (_.valid) map apply

  def move(action: Action): String = action match {
    case ss: SelectSquares => ss.toUci.uci
    case p: Pass           => p.toUci.uci
    case d: Drop           => d.toUci.uci
  }

}
