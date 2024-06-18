package strategygames.chess.format

import strategygames.ActionStrs
import strategygames.chess.variant.Variant
import strategygames.Player.P1

object Sgf {

  private def actionStrsToSGFPos(actionStrs: ActionStrs): List[String] =
    actionStrs.map { turnActions =>
      turnActions
        .flatMap(Uci.apply(_).flatMap {
          case uci: Uci.Move => Some(uci.orig.sgf + uci.dest.sgf)
          case _             => None
        })
        .toList
        .mkString("")
    }.toList

  private def indexToPlayer(whiteStarts: Boolean, index: Int): Char =
    if ((index % 2 == 0 && whiteStarts) || (index % 2 == 1 && !whiteStarts)) 'W' else 'B'

  private def sgfPosWithPlayer(sgfPos: List[String], whiteStarts: Boolean): List[(Char, String)] =
    sgfPos.zipWithIndex.map { case (pos, index) => (indexToPlayer(whiteStarts, index), pos) }

  private def turnToString(posWithPlayer: (Char, String)): String =
    s";${posWithPlayer._1}[${posWithPlayer._2}]"

  def actionStrsToOutput(variant: Variant, actionStrs: ActionStrs, d: Int = 0): String =
    sgfPosWithPlayer(
      actionStrsToSGFPos(actionStrs),
      variant.gameFamily.playerColors.get(P1) == Some("white")
    )
      .map(turnToString)
      .drop(d)
      .mkString("\n")

}
