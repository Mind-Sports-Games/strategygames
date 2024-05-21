package strategygames.go.format

import strategygames.ActionStrs
import strategygames.go.variant.Variant

object Sgf {

  private def actionStrsToSGFPos(variant: Variant, actionStrs: ActionStrs): List[String] =
    actionStrs.flatten.flatMap { str =>
      Uci.apply(str).flatMap {
        case uci: Uci.Drop => Some(uci.pos.sgf(variant.boardSize.height))
        case _: Uci.Pass   => Some("")
        case _             => None
      }
    }.toList

  private def indexToPlayer(index: Int): Char = if (index % 2 == 0) 'B' else 'W'

  private def sgfPosWithPlayer(sgfPos: List[String]): List[(Char, String)] =
    sgfPos.zipWithIndex.map { case (pos, index) => (indexToPlayer(index), pos) }

  private def turnToString(posWithPlayer: (Char, String)): String =
    s";${posWithPlayer._1}[${posWithPlayer._2}]"

  def actionStrsToOutput(variant: Variant, actionStrs: ActionStrs, d: Int = 0) =
    sgfPosWithPlayer(actionStrsToSGFPos(variant, actionStrs)).map(turnToString).drop(d).mkString("\n")

}
