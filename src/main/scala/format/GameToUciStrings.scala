package strategygames.format

import cats.data.Validated

import strategygames.variant.Variant
import strategygames.{ ActionStrs, GameLogic }

object GameToUciStrings {

  def apply(
      lib: GameLogic,
      actionStrs: ActionStrs,
      initialFen: Option[FEN],
      variant: Variant
  ): Validated[String, String] = lib match {
    case GameLogic.Go() | GameLogic.Samurai() | GameLogic.Togyzkumalak() | GameLogic.Abalone() =>
      Validated.valid(join(actionStrs))
    case GameLogic.Backgammon()                                                                 =>
      variant match {
        case Variant.Backgammon(v) =>
          strategygames.backgammon.format.GameToUciStrings(actionStrs, initialFen.map(_.toBackgammon), v)
        case _                     =>
          UciDump(lib, actionStrs, initialFen, variant).map(join)
      }
    case _                                                                                      =>
      UciDump(lib, actionStrs, initialFen, variant).map(join)
  }

  private def join(actionStrs: ActionStrs): String =
    actionStrs.map(_.mkString(",")).mkString(" ")
}
