package strategygames.backgammon

import cats.data.Validated
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import strategygames.backgammon.format.Uci
import strategygames.backgammon.variant.Variant

import strategygames.Player

class BackgammonTest extends Specification with ValidatedMatchers {

  def playUciList(game: Game, ucis: List[Uci]): Validated[String, Game] =
    ucis.foldLeft(Validated.valid(game): Validated[String, Game]) { (vg, action) =>
      vg.flatMap { g => g.apply(action).map(_._1) }
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

}
