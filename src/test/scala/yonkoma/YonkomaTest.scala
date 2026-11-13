package strategygames.yonkoma

import cats.data.Validated
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import strategygames.yonkoma.format.{ FEN, Uci }
import strategygames.yonkoma.variant.Variant

class YonkomaTest extends Specification with ValidatedMatchers {

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

  def playActionStrs(actionStrs: String, variant: Variant): Validated[String, Game] =
    playActionStrs(actionStrs.split(' ').toList, None, Some(variant))

  // a situation from a board part and the player to move, on the given variant
  def situation(
      boardPart: String,
      player: strategygames.Player,
      variant: Variant = Variant.default
  ): Situation =
    Situation(Board(FEN(boardPart).pieces, variant), player)

  def pos(key: String): Pos = Pos.fromKey(key).getOrElse(sys.error(s"bad key ${key}"))

  // specs2 puts its own apply/count on Seq, so index by dropping rather than by apply
  def pick[A](rng: scala.util.Random, as: Seq[A]): A = as.drop(rng.nextInt(as.size)).head

  def destinationKeys(s: Situation, from: String): List[String] =
    s.destinations.getOrElse(pos(from), Nil).map(_.key)
}
