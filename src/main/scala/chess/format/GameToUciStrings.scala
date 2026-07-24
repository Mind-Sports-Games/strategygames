package strategygames.chess.format

import scala.annotation.tailrec
import cats.data.Validated

import strategygames.{
  Action => StratAction,
  ActionStrs,
  Drop => StratDrop,
  Move => StratMove,
  Situation => StratSituation
}
import strategygames.chess.{ Action, Situation }
import strategygames.chess.format.pgn.Parser
import strategygames.chess.variant.Variant
import strategygames.format.pgn.San

object GameToUciStrings {

  def apply(
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: Variant
  ): Validated[String, String] =
    if (actionStrs.isEmpty) Validated.valid("")
    else {
      val start = initialFen.flatMap(Forsyth.<<).getOrElse(Situation(variant)).withVariant(variant)
      Parser
        .sans(actionStrs.flatten, variant)
        .andThen(sans => convert(start, sans.value, variant, Nil))
        .map(regroup(actionStrs, _))
    }

  @tailrec
  private def convert(
      situation: Situation,
      sans: List[San],
      variant: Variant,
      acc: List[String]
  ): Validated[String, List[String]] =
    sans match {
      case Nil         => Validated.valid(acc.reverse)
      case san :: rest =>
        san(StratSituation.wrap(situation)).map(chessAction) match {
          case Validated.Valid(action)   =>
            val next = Situation(clearPositionHashes(action.finalizeAfter), !situation.player)
            convert(next, rest, variant, UciDump.action(variant)(action) :: acc)
          case invalid @ Validated.Invalid(_) => invalid
        }
    }

  private def chessAction(action: StratAction): Action =
    action match {
      case StratMove.Chess(m) => m
      case StratDrop.Chess(d) => d
      case _                  => sys.error("Invalid chess action")
    }

  private def clearPositionHashes(board: strategygames.chess.Board): strategygames.chess.Board =
    board.updateHistory(_.copy(positionHashes = Array.empty[Byte]))

  private def regroup(actionStrs: ActionStrs, flat: List[String]): String =
    actionStrs
      .foldLeft((Vector.empty[String], flat)) { case ((groups, remaining), turn) =>
        val (head, tail) = remaining.splitAt(turn.size)
        (groups :+ head.mkString(","), tail)
      }
      ._1
      .mkString(" ")
}
