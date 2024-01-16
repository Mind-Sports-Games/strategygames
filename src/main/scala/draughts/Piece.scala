package strategygames.draughts

import strategygames.Player

case class Piece(player: Player, role: Role) {

  def is(c: Player)    = c == player
  def is(r: Role)      = r == role
  def isNot(c: Player) = c != player
  def isNot(r: Role)   = r != role

  def oneOf(rs: Set[Role]) = rs(role)

  def isGhost = role == GhostMan || role == GhostKing

  def forsyth: Char = role.forsyth

  def ghostRole =
    if (role == Man) GhostMan
    else if (role == King) GhostKing
    else role

  override def toString = s"${player}-${role}".toLowerCase
}

object Piece {

  def fromChar(c: Char): Option[Piece] =
    Role.allByPdn get c.toUpper map {
      Piece(Player.fromP1(c.isUpper), _)
    }

}
