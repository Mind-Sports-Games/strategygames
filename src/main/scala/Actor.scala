package strategygames

import format.Uci
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

sealed class Actor(
  val piece: Piece,
  val pos: Pos,
  val board: Board
) {

  def color: Color          = piece.color
  def is(c: Color): Boolean = c == piece.color
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

}
