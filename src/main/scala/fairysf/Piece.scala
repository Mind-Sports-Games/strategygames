package strategygames.fairysf

import strategygames.{ GameFamily, Player }

case class Piece(player: Player, role: Role) {

  def is(c: Player)    = c == player
  def is(r: Role)      = r == role
  def isNot(c: Player) = c != player
  def isNot(r: Role)   = r != role

  def oneOf(rs: Set[Role]) = rs(role)

  def forsyth: Char     = if (player == P1) role.forsythUpper else role.forsyth.toLower
  override def toString = s"$player-$role".toLowerCase
}

object Piece {

  def fromChar(gf: GameFamily, c: Char): Option[Piece] =
    Role.allByPgn(gf) get c.toUpper map {
      Piece(Player.fromP1(c.isUpper), _)
    }

}
