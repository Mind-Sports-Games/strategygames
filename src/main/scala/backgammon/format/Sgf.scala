package strategygames.backgammon.format

import strategygames.{ ActionStrs, Player }

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

  private def indexToPlayer(index: Int, startPlayer: Player): Char =
    if (index % 2 == startPlayer.hashCode - 1) 'W' else 'B'

  private def sgfWithPlayer(sgf: List[String], startPlayer: Player): List[(Char, String)] =
    sgf.zipWithIndex.map { case (notation, index) => (indexToPlayer(index, startPlayer), notation) }

  private def turnToString(turnWithPlayer: (Char, String)): String =
    s";${turnWithPlayer._1}[${turnWithPlayer._2}]"

  def actionStrsToOutput(actionStrs: ActionStrs, initialFen: Option[FEN], d: Int = 0) =
    sgfWithPlayer(actionStrsToSGF(actionStrs), initialFen.flatMap(_.player).getOrElse(Player.P1))
      .map(turnToString)
      .drop(d)
      .mkString("\n")

}
