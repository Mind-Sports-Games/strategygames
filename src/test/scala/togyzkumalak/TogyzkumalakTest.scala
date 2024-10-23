package strategygames.togyzkumalak

import cats.data.Validated
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import strategygames.togyzkumalak.format.Uci
import strategygames.togyzkumalak.variant.Variant

class TogyzkumalakTest extends Specification with ValidatedMatchers {

  def playUciList(game: Game, ucis: List[Uci]): Validated[String, Game] =
    ucis.foldLeft(Validated.valid(game): Validated[String, Game]) { (vg, action) =>
      action match {
        case action: Uci.Move => vg.flatMap { g => g.apply(action).map(_._1) }
        case _                => sys.error("Invalid Uci type")
      }
    }

  def playActionStrs(
      actionStrs: List[String],
      game: Option[Game] = None,
      variant: Option[Variant] = None
  ): Validated[String, Game] =
    playUciList(
      game.getOrElse(Game.apply(variant.getOrElse(Variant.default))),
      Uci.readList(actionStrs.mkString(" ")).getOrElse(List())
    )

}
