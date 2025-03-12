package strategygames.abalone

import strategygames.Player

case class Piece(player: Player, role: Role) {
  def is(p: Player) = p == player

  def is(r: Role) = r == role

  def isNot(p: Player) = p != player

  def isNot(r: Role) = r != role

  def oneOf(roles: Set[Role]) = roles(role)

  def forsyth: Char = role.forsyth

  override def toString = s"${player.toString.toLowerCase}-$role"
}

object Piece {
  // Is it wrong??
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