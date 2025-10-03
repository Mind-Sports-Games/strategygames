package strategygames.fairysf

import strategygames.{ ClockBase, Player }

import cats.data.Validated
import cats.syntax.option._
import org.specs2.matcher.Matcher
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification

import strategygames.fairysf.format.{ FEN, Forsyth, Uci, Visual }
import strategygames.fairysf.variant.Variant

trait FairySFTest extends Specification with ValidatedMatchers {

  implicit def stringToBoard(str: String): Board = Visual << str

  // implicit def stringToBoardBuilder(str: String) =
  //  new {

  //  }

  implicit def stringToSituationBuilder(str: String) =
    new {

      def as(player: Player): Situation = Situation(Visual << str, player)
    }

  // case class RichActor(actor: Actor) {
  //  def threatens(to: Pos): Boolean =
  //    actor.piece.eyes(actor.pos, to) && {
  //      (!actor.piece.role.projection) ||
  //      actor.piece.role.dir(actor.pos, to).exists {
  //        Actor.longRangeThreatens(actor.board, actor.pos, _, to)
  //      }
  //    }
  // }

  // implicit def richActor(actor: Actor) = RichActor(actor)

  case class RichGame(game: Game) {

    def playMoves(moves: (Pos, Pos)*): Validated[String, Game] = playMoveList(moves)

    def playMoveList(moves: Iterable[(Pos, Pos)]): Validated[String, Game] = {
      val vg = moves.foldLeft(Validated.valid(game): Validated[String, Game]) { (vg, move) =>
        // vg foreach { x =>
        // println(s"------------------------ ${x.turns} = $move")
        // }
        // because possible moves are asked for player highlight
        // before the move is played (on initial situation)
        // vg foreach { _.situation.destinations }
        val ng = vg flatMap { g =>
          g(move._1, move._2) map (_._1)
        }
        ng
      }
      // vg foreach { x => println("========= PGN: " + x.pgnMoves) }
      vg
    }

    def playMove(
        orig: Pos,
        dest: Pos,
        promotion: Option[PromotableRole] = None
    ): Validated[String, Game] =
      game.apply(orig, dest, promotion) map (_._1)

    def withClock(c: ClockBase) = game.copy(clock = Option(c))
  }

  implicit def richGame(game: Game) = RichGame(game)

  // this isnt how we initialise games for fairy and so isn't a good test
  // It doesn't work all the time either
  def fenToGame(positionString: FEN, variant: Variant) = {
    val situation = Forsyth.<<@(variant, positionString)
    situation map { sit =>
      sit.player -> sit.withVariant(variant).board
    } toValid "Could not construct situation from FEN" map { case (player, board) =>
      Game(variant).copy(
        situation = Situation(board, player)
      )
    }
  }

  def makeBoard(pieces: (Pos, Piece)*): Board =
    Board(pieces toMap, History(), strategygames.fairysf.variant.Shogi)

  def makeBoard(str: String, variant: Variant) =
    Visual << str withVariant variant

  def makeBoard: Board = Board init strategygames.fairysf.variant.Shogi

  def bePoss(poss: Pos*): Matcher[Option[Iterable[Pos]]] =
    beSome.like { case p =>
      sortPoss(p.toList) must_== sortPoss(poss.toList)
    }

  def bePoss(board: Board, visual: String): Matcher[Option[Iterable[Pos]]] =
    beSome.like { case p =>
      Visual.addNewLines(Visual.>>|(board, Map(p -> 'x'))) must_== visual
    }

  def sortPoss(poss: Seq[Pos]): Seq[Pos] = poss sortBy (_.toString)

  def playUciList(game: Game, ucis: List[Uci]): Validated[String, Game] =
    ucis.foldLeft(Validated.valid(game): Validated[String, Game]) { (vg, action) =>
      vg.flatMap { g => g.apply(action).map(_._1) }
    }

  def playActionStrs(
    actionStrs: List[String],
    game: Option[Game] = None,
    variant: Option[Variant] = None
  ): Validated[String, Game] = {
    val g = game.getOrElse(Game.apply(variant.getOrElse(Variant.default)))
    playUciList(
      g,
      Uci.readList(g.situation.board.variant.gameFamily, actionStrs.mkString(" ")).getOrElse(List())
    )
  }

}
