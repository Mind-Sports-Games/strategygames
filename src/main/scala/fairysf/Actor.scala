package strategygames.fairysf

import strategygames.Color

import format.Uci
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

final case class Actor(
    piece: Piece,
    pos: Pos,
    board: Board
) {

  import Actor._

  def color        = piece.color
  def is(c: Color) = c == piece.color
  def is(r: Role)  = r == piece.role
  def is(p: Piece) = p == piece
}
