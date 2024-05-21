package strategygames.fairysf.format

import strategygames.{ ActionStrs, GameFamily }
import strategygames.fairysf.variant.Variant
import strategygames.Player.P1

object Sgf {

  def actionStrsToSGFPos(variant: Variant, actionStrs: ActionStrs): List[String] =
    actionStrs.map { turnActions =>
      turnActions
        .flatMap(Uci.apply(variant.gameFamily, _).flatMap {
          case uci: Uci.Drop                         =>
            variant.gameFamily match {
              case GameFamily.Shogi() => Some("*" + uci.role.pgn + uci.pos.sgf(variant.boardSize.height))
              case _                  => Some(uci.pos.sgf(variant.boardSize.height))
            }
          case uci: Uci.Move if uci.orig == uci.dest => Some("") // pass in othello
          case uci: Uci.Move                         =>
            Some(
              uci.orig.sgf(variant.boardSize.height) + uci.dest
                .sgf(variant.boardSize.height) + uci.sgfPromotionString
            )
          case _                                     => None
        })
        .toList
        .mkString("")
    }.toList

  def indexToPlayer(whiteStarts: Boolean, index: Int): Char =
    if ((index % 2 == 0 && whiteStarts) || (index % 2 == 1 && !whiteStarts)) 'W' else 'B'

  def sgfPosWithPlayer(sgfPos: List[String], whiteStarts: Boolean): List[(Char, String)] =
    sgfPos.zipWithIndex.map { case (pos, index) => (indexToPlayer(whiteStarts, index), pos) }

  def turnToString(posWithPlayer: (Char, String)): String =
    s";${posWithPlayer._1}[${posWithPlayer._2}]"

  def actionStrsToOutput(variant: Variant, actionStrs: ActionStrs, d: Int = 0) =
    sgfPosWithPlayer(
      actionStrsToSGFPos(variant, actionStrs),
      variant.gameFamily.playerColors.get(P1) == Some("white")
    )
      .map(turnToString)
      .drop(d)
      .mkString("\n")

}
