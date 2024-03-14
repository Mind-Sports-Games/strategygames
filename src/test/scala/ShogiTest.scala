package strategygames

import cats.data.Validated
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

//import scala.annotation.nowarn

import strategygames.fairysf.variant._
import strategygames.fairysf.{ Board, Game, Pos, PromotableRole, Situation }

trait ShogiTest extends Specification with ValidatedMatchers {

  case class RichGame(game: Game) {

    def as(color: Player): Game = game.withPlayer(color)

    def playMoves(moves: (Pos, Pos, Option[PromotableRole])*): Validated[String, Game] = playMoveList(moves)

    def playMoveList(moves: Seq[(Pos, Pos, Option[PromotableRole])]): Validated[String, Game] = {
      val vg = moves.foldLeft[Validated[String, Game]](Validated.valid(game)) {
        case (vg, (orig, dest, prom)) =>
          vg.foreach { g =>
            val _ = g.situation.destinations
          }
          val ng = vg.flatMap { g =>
            g(orig, dest, prom)
          }
          ng.map(t => t._1)
      }
      vg
    }

    def playMove(
        orig: Pos,
        dest: Pos,
        promotion: Option[PromotableRole] = None
    ): Validated[String, Game] =
      game.apply(orig, dest, promotion).map(t => t._1)

    // def playDrop(
    // role: Role,
    // dest: Pos
    // ): Validated[String, Game] =
    // game.applyDrop(Uci.Drop(role, dest))

    def withClock(c: ClockBase) = game.copy(clock = Some(c))
  }

  implicit def richGame(game: Game) = RichGame(game)

  def makeSituation: Situation = Situation(Shogi)

  def makeEmptySituation: Situation                   = Situation(Shogi).copy(board = Board.empty(Shogi))
  def makeEmptySituation(variant: Variant): Situation =
    Situation(variant).copy(board = Board.empty(variant))

  def makeGame: Game = Game(makeSituation)

}
