package strategygames.go.format

import strategygames.ActionStrs

object Sgf {

  def actionStrsToSGFPos(actionStrs: ActionStrs): List[String] =
    actionStrs.flatten
      .map { str =>
        Uci.apply(str).map { case uci: Uci.Drop => uci.pos.sgf }
      }
      .flatten
      .toList

  def indexToPlayer(index: Int): Char = if (index % 2 == 0) 'B' else 'W'

  def sgfPosWithPlayer(sgfPos: List[String]): List[(Char, String)] =
    sgfPos.zipWithIndex.map { case (pos, index) => (indexToPlayer(index), pos) }

  def turnToString(posWithPlayer: (Char, String)): String =
    s";${posWithPlayer._1}[${posWithPlayer._2}]"

  def actionStrsToOutput(actionStrs: ActionStrs, d: Int = 0) =
    sgfPosWithPlayer(actionStrsToSGFPos(actionStrs)).map(turnToString).drop(d).mkString("\n")

}
