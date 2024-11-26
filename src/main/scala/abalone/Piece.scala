package strategygames.abalone

import strategygames.Player

case class Piece(player: Player, role: Role) {

  def is(c: Player)    = c == player
  def is(r: Role)      = r == role
  def isNot(c: Player) = c != player
  def isNot(r: Role)   = r != role

  def oneOf(rs: Set[Role]) = rs(role)

  def forsyth: Char     = role.forsyth
  override def toString = s"${player.toString.toLowerCase}-$role"
}

object Piece {

  // is it wrong ??
  def fromChar(c: Char): Option[Piece] =
    Role.allByPgn get c.toUpper map {
      Piece(Player.fromP1(c.isUpper), _)
    }

  // This is wrong
  // def fromChar(gf: GameFamily, c: Char): Option[Piece] =
  //  Role.allByPgn(gf) get c.toUpper map {
  //    Piece(Player.fromP1(c.isUpper), _)
  //  }

  // This is now wrong when changing fen format for Oware to include count
  // def fromStoneNumber(player: Player, n: Int): Option[Piece] =
  //  Role.allByBinaryInt get n map {
  //    Piece(player, _)
  //  }

}
