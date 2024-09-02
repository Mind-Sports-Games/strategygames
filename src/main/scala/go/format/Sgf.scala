package strategygames.go.format

import strategygames.ActionStrs
import strategygames.go.variant.Variant
import strategygames.Player

object Sgf {

  private def actionStrsToSGFPos(variant: Variant, actionStrs: ActionStrs): List[String] =
    actionStrs.flatten.flatMap { str =>
      Uci.apply(str).flatMap {
        case uci: Uci.Drop => Some(uci.pos.sgf(variant.boardSize.height))
        case _: Uci.Pass   => Some("")
        case _             => None
      }
    }.toList

  private def initialMoves(variant: Variant, initialFen: FEN): List[(Char, String)] =
    strategygames.go.Api
      .pieceMapFromFen(variant.key, initialFen.value)
      .toList
      .map { case (pos, piece) =>
        (if (piece.player == Player.P1) 'B' else 'W', pos.sgf(variant.boardSize.height))
      }
      .sortBy(_._2)

  private def indexToPlayer(index: Int, isHandicapGame: Boolean): Char =
    if ((index % 2 == 0 && !isHandicapGame) || (index % 2 == 1 && isHandicapGame)) 'B' else 'W'

  private def sgfPosWithPlayer(sgfPos: List[String], isHandicapGame: Boolean): List[(Char, String)] =
    sgfPos.zipWithIndex.map { case (pos, index) => (indexToPlayer(index, isHandicapGame), pos) }

  private def turnToString(posWithPlayer: (Char, String)): String =
    s";${posWithPlayer._1}[${posWithPlayer._2}]"

  def actionStrsToOutput(
      variant: Variant,
      actionStrs: ActionStrs,
      initialFen: Option[FEN] = None,
      d: Int = 0
  ) =
    (initialFen.fold(List.empty[(Char, String)])(f => initialMoves(variant, f)) ::: sgfPosWithPlayer(
      actionStrsToSGFPos(variant, actionStrs),
      initialFen.fold(false)(f => f.handicap.fold(false)(h => h > 0))
    )).map(turnToString).drop(d).mkString("\n")

}
