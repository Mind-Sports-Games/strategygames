package strategygames.backgammon

import strategygames.Player

case class Piece(player: Player, role: Role) {

  def is(c: Player)    = c == player
  def is(r: Role)      = r == role
  def isNot(c: Player) = c != player
  def isNot(r: Role)   = r != role

  def oneOf(rs: Set[Role]) = rs(role)

  def forsyth: Char     = player.fold(role.forsythUpper, role.forsyth)
  override def toString = s"${player.toString.toLowerCase}-$role"

}

object Piece {

  // This is only currently used for the pockets
  def fromChar(c: Char): Option[Piece] =
    Role.allByPgn get c.toUpper map {
      Piece(Player.fromP1(c.isUpper), _)
    }

}
