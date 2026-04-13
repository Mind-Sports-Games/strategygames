package strategygames.fairysf.format

import strategygames.{ ActionStrs, GameFamily }
import strategygames.fairysf.variant.Variant
import strategygames.Player.P1

object Sgf {

  private def actionStrsToSGFPos(variant: Variant, actionStrs: ActionStrs): List[String] =
    actionStrs.map { turnActions =>
      turnActions
        .flatMap(a =>
          {
            (a, variant.gameFamily) match {
              case (s, GameFamily.Shogi()) if s.takeRight(1) == "+" =>
                Uci.Move(variant.gameFamily, a.dropRight(1) ++ "P") // require a role to get promotion output
              case (_, _)                                           => Uci.apply(variant.gameFamily, a)
            }
          }.flatMap {
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
          }
        )
        .toList
        .mkString("")
    }.toList

  private def indexToPlayer(whiteStarts: Boolean, index: Int): Char =
    if ((index % 2 == 0 && whiteStarts) || (index % 2 == 1 && !whiteStarts)) 'W' else 'B'

  private def sgfPosWithPlayer(sgfPos: List[String], whiteStarts: Boolean): List[(Char, String)] =
    sgfPos.zipWithIndex.map { case (pos, index) => (indexToPlayer(whiteStarts, index), pos) }

  private def turnToString(posWithPlayer: (Char, String)): String =
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
