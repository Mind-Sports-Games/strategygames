package strategygames.dameo

import strategygames.Player

case class Piece(player: Player, role: Role) {

  def is(c: Player)    = c == player
  def is(r: Role)      = r == role
  def isNot(c: Player) = c != player
  def isNot(r: Role)   = r != role

  def oneOf(rs: Set[Role]) = rs(role)

  def forsyth: Char = player.fold(role.forsythUpper, role.forsyth)

  def isGhost = role == GhostMan || role == GhostKing

  def isActive = role == ActiveMan || role == ActiveKing

  def ghostRole =
    if (role == Man) GhostMan
    else if (role == King) GhostKing
    else role

  def activeRole =
    if (role == Man) ActiveMan
    else if (role == King) ActiveKing
    else role

  def unactiveRole =
    if (role == ActiveMan) Man
    else if (role == ActiveKing) King
    else role

  override def toString = s"${player.toString.toLowerCase}-$role"

}

object Piece {

  // This is only currently used for the pockets
  def fromChar(c: Char): Option[Piece] =
    Role.allByPdn get c.toLower map {
      Piece(Player.fromP1(c.isUpper), _)
    }

}
