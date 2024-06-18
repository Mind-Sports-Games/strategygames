package strategygames.backgammon.format

import strategygames.ActionStrs

object Sgf {

  private def actionStrsToSGF(actionStrs: ActionStrs): List[String] =
    actionStrs.map { turnActions =>
      turnActions
        .flatMap(Uci.apply(_).flatMap {
          case uci: Uci.Drop     => Some("y" + uci.pos.sgf)
          case uci: Uci.Move     => Some(uci.orig.sgf + uci.dest.sgf)
          case uci: Uci.Lift     => Some(uci.pos.sgf + "z")
          case uci: Uci.DiceRoll => Some(uci.dice.take(2).sorted.reverse.mkString(""))
          case _                 => None
        })
        .toList
        .mkString("")
    }.toList

  private def indexToPlayer(index: Int): Char = if (index % 2 == 0) 'W' else 'B'

  private def sgfWithPlayer(sgf: List[String]): List[(Char, String)] =
    sgf.zipWithIndex.map { case (notation, index) => (indexToPlayer(index), notation) }

  private def turnToString(turnWithPlayer: (Char, String)): String =
    s";${turnWithPlayer._1}[${turnWithPlayer._2}]"

  def actionStrsToOutput(actionStrs: ActionStrs, d: Int = 0) =
    sgfWithPlayer(actionStrsToSGF(actionStrs)).map(turnToString).drop(d).mkString("\n")

}
