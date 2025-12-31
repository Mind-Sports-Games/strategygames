package strategygames.backgammon

import cats.data.Validated
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import strategygames.backgammon.format.Uci
import strategygames.backgammon.variant.Variant

import strategygames.Player

// Helper object OUTSIDE of specs2 scope to avoid implicit conversion issues
object BackgammonTestUtils {
  def movesToUciSet(moves: Iterable[Move]): Set[String] =
    moves.toList.map(_.toUci.uci).toSet

  def liftsToUciSet(lifts: List[Lift]): Set[String] =
    lifts.map(_.toUci.uci).toSet

  def dropsToUciSet(drops: List[Drop]): Set[String] =
    drops.map(_.toUci.uci).toSet

  def dropsToUciList(drops: List[Drop]): List[String] =
    drops.map(_.toUci.uci)

  def actionsToUciList(actions: List[Action]): List[String] =
    actions.map(_.toUci.uci)
}

class BackgammonTest extends Specification with ValidatedMatchers {

  def playUciList(game: Game, ucis: List[Uci]): Validated[String, Game] =
    ucis.foldLeft(Validated.valid(game): Validated[String, Game]) { (vg, action) =>
      vg.andThen { g => g.apply(action).map(_._1) }
    }

  def playActionStrs(
      actionStrs: List[String],
      game: Option[Game] = None,
      variant: Option[Variant] = None,
      startPlayer: Player = Player.P1
  ): Validated[String, Game] =
    playUciList(
      game.getOrElse(Game.makeGame(variant.getOrElse(Variant.default), Some(startPlayer))),
      Uci.readList(actionStrs.mkString(" ")).getOrElse(List())
    )

  def forceValidGameToGame(validGame: Validated[String, Game]): Game =
    validGame.toOption.head

  // Helper to convert moves to UCI strings - avoids specs2 implicit conversion issues
  def movesToUciSet(moves: Iterable[Move]): Set[String] =
    moves.toList.map(_.toUci.uci).toSet

  def liftsToUciSet(lifts: List[Lift]): Set[String] =
    lifts.map(_.toUci.uci).toSet

  def dropsToUciSet(drops: List[Drop]): Set[String] =
    drops.map(_.toUci.uci).toSet

  def dropsToUciList(drops: List[Drop]): List[String] =
    drops.map(_.toUci.uci)

  def actionsToUciList(actions: List[Action]): List[String] =
    actions.map(_.toUci.uci)

}

