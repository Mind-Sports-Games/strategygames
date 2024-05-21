package strategygames.format

import strategygames._
import strategygames.variant.Variant

object Sgf {

  def actionStrsToOutput(variant: Variant, actionStrs: ActionStrs, d: Int = 0): String =
    variant match {
      case Variant.Chess(variant) if variant.gameFamily == GameFamily.LinesOfAction() =>
        chess.format.Sgf.actionStrsToOutput(variant, actionStrs, d)
      case Variant.FairySF(variant)                                                   => fairysf.format.Sgf.actionStrsToOutput(variant, actionStrs, d)
      case Variant.Go(variant)                                                        => go.format.Sgf.actionStrsToOutput(variant, actionStrs, d)
      case Variant.Backgammon(_)                                                      => backgammon.format.Sgf.actionStrsToOutput(actionStrs, d)
      case g                                                                          => sys.error(s"sgf not supported for variant ${g}")
    }
}
