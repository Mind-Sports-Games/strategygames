package strategygames.fairysf

import strategygames.{ Color, GameFamily }

case class Piece(color: Color, role: Role) {

  def is(c: Color)    = c == color
  def is(r: Role)     = r == role
  def isNot(c: Color) = c != color
  def isNot(r: Role)  = r != role

  def oneOf(rs: Set[Role]) = rs(role)

  def forsyth: Char = if (color == White) role.forsythUpper else role.forsyth
  override def toString = s"$color-$role".toLowerCase
}

object Piece {

  def fromChar(gf: GameFamily, c: Char): Option[Piece] =
    Role.allByPgn(gf) get c.toUpper map {
      Piece(Color.fromWhite(c.isUpper), _)
    }

}
