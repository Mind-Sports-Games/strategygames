package strategygames.entropy

import strategygames.Player

case class Piece(player: Player, role: Role) {

  def is(c: Player)    = c == player
  def is(r: Role)      = r == role
  def isNot(c: Player) = c != player
  def isNot(r: Role)   = r != role

  def oneOf(rs: Set[Role]) = rs(role)

  def forsyth: Char = player.fold(role.forsythUpper, role.forsyth)

  // a counter belongs to whoever may act on it, so it changes hands as it lands
  def ownedBy(player: Player): Piece = copy(player = player)

  override def toString = s"${player.toString.toLowerCase}-$role"

}

object Piece {

  def fromChar(c: Char): Option[Piece] =
    Role.allByForsyth get c.toLower map {
      Piece(Player.fromP1(c.isUpper), _)
    }

}
