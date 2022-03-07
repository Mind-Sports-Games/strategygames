package strategygames

abstract sealed class Actor(
  val piece: Piece,
  val pos: Pos,
  val board: Board
) {

  def player: Player          = piece.player
  def is(c: Player): Boolean = c == piece.player
  def is(r: Role): Boolean  = r == piece.role
  def is(p: Piece): Boolean = p == piece

}

object Actor {

  final case class Chess(a: chess.Actor) extends Actor(
    Piece.Chess(a.piece),
    Pos.Chess(a.pos),
    Board.Chess(a.board)
  ) {}

  final case class Draughts(a: draughts.Actor) extends Actor(
    Piece.Draughts(a.piece),
    Pos.Draughts(a.pos),
    Board.Draughts(a.board)
  ){}

  final case class FairySF(a: fairysf.Actor) extends Actor(
    Piece.FairySF(a.piece),
    Pos.FairySF(a.pos),
    Board.FairySF(a.board)
  ){}

}
