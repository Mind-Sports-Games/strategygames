package strategygames.abalone

import org.specs2.matcher.ValidatedMatchers

trait IAbaloneTest extends ValidatedMatchers {
  def of1(board: Board): Move => Boolean   = m => board.variant.boardType.norm(m.dest - m.orig) == 1
  def ofGt1(board: Board): Move => Boolean = m => board.variant.boardType.norm(m.dest - m.orig) > 1

  def valid(game: Game): Map[Pos, List[Move]]          = valid(game.situation)
  def valid(sit: Situation): Map[Pos, List[Move]]      = sit.board.variant.validMoves(sit)
  def valid_line(sit: Situation): Map[Pos, List[Move]] = sit.board.variant.validMoves_line(sit)
  def valid_jump(sit: Situation): Map[Pos, List[Move]] = sit.board.variant.validMoves_jump(sit)

  def next(game: Game, fx: Int, fy: Int, tx: Int, ty: Int): Game                                   = next(game, valid(game), fx, fy, tx, ty)
  def next(game: Game, orig: Pos, dest: Pos): Game                                                 = next(game, valid(game), orig, dest)
  def next(game: Game, validMoves: Map[Pos, List[Move]], fx: Int, fy: Int, tx: Int, ty: Int): Game =
    next(game, validMoves, new Pos(fx, fy), new Pos(tx, ty))
  def next(game: Game, validMoves: Map[Pos, List[Move]], orig: Pos, dest: Pos): Game               =
    game(validMoves(orig).find(m => m.dest == dest).get)

  def ukeys(validMoves: Map[Pos, List[Move]], fx: Int, fy: Int): Iterable[String] =
    ukeys(validMoves, new Pos(fx, fy))
  def ukeys(validMoves: Map[Pos, List[Move]], orig: Pos): Iterable[String]        = ukeys(validMoves.get(orig).get)
  def ukeys(moves: Iterable[Move]): Iterable[String]                              = moves.map(_.toUci.keys)
}
