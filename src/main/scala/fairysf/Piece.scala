package strategygames.fairysf

import strategygames.Color

case class Piece(color: Color, role: Role) {

  def is(c: Color)    = c == color
  def is(r: Role)     = r == role
  def isNot(c: Color) = c != color
  def isNot(r: Role)  = r != role

  def oneOf(rs: Set[Role]) = rs(role)

  def isGhost = role == GhostMan || role == GhostKing

  def forsyth: Char = role.forsyth

  def ghostRole =
    if (role == Man) GhostMan
    else if (role == King) GhostKing
    else role

  override def toString = (color + "-" + role).toLowerCase
}

object Piece {

  def fromChar(c: Char): Option[Piece] =
    Role.allByPdn get c.toUpper map {
      Piece(Color.fromWhite(c.isUpper), _)
    }

}
