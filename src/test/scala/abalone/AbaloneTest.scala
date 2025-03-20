package strategygames.abalone

import cats.data.Validated
import org.specs2.matcher.ValidatedMatchers
import org.specs2.mutable.Specification
import strategygames.abalone.format.Uci
import strategygames.abalone.variant.Variant

class AbaloneTest extends Specification with ValidatedMatchers {
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

  def of1(board: Board): Move => Boolean = m => board.variant.boardType.norm(m.dest - m.orig) == 1

  def ofGt1(board: Board): Move => Boolean = m => board.variant.boardType.norm(m.dest - m.orig) > 1

  def valid(game: Game): Map[Pos, List[Move]] = valid(game.situation)

  def valid(sit: Situation): Map[Pos, List[Move]] = sit.board.variant.validMoves(sit)

  def valid_line(sit: Situation): Map[Pos, List[Move]] = sit.board.variant.validMoves_line(sit)

  def valid_jump(sit: Situation): Map[Pos, List[Move]] = sit.board.variant.validMoves_jump(sit)

  def next(game: Game, fx: Int, fy: Int, tx: Int, ty: Int): Game =
    next(game, valid(game), fx, fy, tx, ty)

  def next(game: Game, validMoves: Map[Pos, List[Move]], fx: Int, fy: Int, tx: Int, ty: Int): Game =
    next(game, validMoves, new Pos(fx, fy), new Pos(tx, ty))

  def next(game: Game, validMoves: Map[Pos, List[Move]], orig: Pos, dest: Pos): Game =
    game(validMoves(orig).find(m => m.dest == dest).get)

  def ukeys(validMoves: Map[Pos, List[Move]], fx: Int, fy: Int): Iterable[String] =
    ukeys(validMoves, new Pos(fx, fy))

  def ukeys(validMoves: Map[Pos, List[Move]], orig: Pos): Iterable[String] = ukeys(validMoves.get(orig).get)

  def ukeys(moves: Iterable[Move]): Iterable[String] = moves.map(_.toUci.keys)
}
