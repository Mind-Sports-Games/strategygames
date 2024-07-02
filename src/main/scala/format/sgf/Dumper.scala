package strategygames
package format.sgf

import strategygames._
import strategygames.format.FEN
import strategygames.variant.Variant

object Dumper {

  def apply(variant: Variant, actionStrs: ActionStrs, initialFen: Option[FEN] = None, d: Int = 0): String =
    variant match {
      case Variant.FairySF(variant) => fairysf.format.Sgf.actionStrsToOutput(variant, actionStrs, d)
      case Variant.Go(variant)      =>
        go.format.Sgf.actionStrsToOutput(variant, actionStrs, initialFen.map(f => f.toGo), d)
      case Variant.Backgammon(_)    => backgammon.format.Sgf.actionStrsToOutput(actionStrs, d)
      case g                        => sys.error(s"sgf not supported for variant ${g}")
    }
}
