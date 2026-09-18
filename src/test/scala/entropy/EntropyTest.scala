package strategygames.entropy

import cats.data.Validated
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import strategygames.entropy.format.Uci
import strategygames.entropy.variant.Variant

class EntropyTest extends Specification with ValidatedMatchers {

  def playUciList(game: Game, ucis: List[Uci]): Validated[String, Game] =
    ucis.foldLeft(Validated.valid(game): Validated[String, Game]) { (vg, action: Uci) =>
      vg.andThen { g => g.apply(action).map(_._1) }
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

  // "rg.gr" -> a line of red, green, empty, green, red
  def line(s: String): Line =
    s.toVector.map(c => if (c == '.') None else Role.allByForsyth.get(c))

  def applyAction(g: Game, a: Action): Game = a match {
    case m: Move         => g.apply(m)
    case d: Drop         => g.apply(d)
    case p: Pass         => g.apply(p)
    case dc: DrawCounter => g.apply(dc)
    case _               => g
  }

  // play the game out with both players flagged, which is a legal sequence of actions
  def playForced(game: Game, limit: Int = 500): Game =
    Iterator
      .iterate(game)(g => g.situation.flaggedAction.fold(g)(a => applyAction(g, a)))
      .take(limit)
      .find(g => g.situation.end || g.situation.flaggedAction.isEmpty)
      .getOrElse(game)
}
