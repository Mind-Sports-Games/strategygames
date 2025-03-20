package strategygames.abalone

import cats.data.Validated
import org.specs2.mutable.Specification
import strategygames.abalone.format.Uci
import strategygames.abalone.variant.Variant

class AbaloneTest extends Specification with IAbaloneTest {
  def playUciList(game: Game, ucis: List[Uci]): Validated[String, Game] =
    ucis.foldLeft(Validated.valid(game): Validated[String, Game]) { (vg, action: Uci) =>
      vg.flatMap { g => g.apply(action).map(_._1) }
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
