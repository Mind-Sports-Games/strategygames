package strategygames.format

import cats.data.Validated

import strategygames.variant.Variant
import strategygames.{ ActionStrs, GameFamily, GameLogic }

object GameToUciStrings {

  def apply(
      lib: GameLogic,
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: Variant
  ): Validated[String, String] = variant match {
    case Variant.Backgammon(v)                                                 =>
      strategygames.backgammon.format.GameToUciStrings(actionStrs, initialFen.map(_.toBackgammon), v)
    case Variant.FairySF(v) if v.gameFamily != GameFamily.Shogi()              => // promotions
      Validated.valid(join(actionStrs))
    case Variant.Go(_) | Variant.Samurai(_) | Variant.Togyzkumalak(_)          =>
      Validated.valid(join(actionStrs))
    case Variant.Abalone(v) if v != strategygames.abalone.variant.GrandAbalone =>
      Validated.valid(join(actionStrs))
    case _                                                                     =>
      UciDump(lib, actionStrs, initialFen, variant).map(join)
  }

  private def join(actionStrs: ActionStrs): String =
    actionStrs.map(_.mkString(",")).mkString(" ")
}
