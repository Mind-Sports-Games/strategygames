package strategygames.yonkoma

import strategygames.Player

case class Piece(player: Player, role: Role) {

  def is(c: Player) = c == player

  def forsyth: Char = player.fold(role.forsythUpper, role.forsyth)

  override def toString = s"${player.toString.toLowerCase}-$role"

}

object Piece {

  def fromChar(c: Char): Option[Piece] =
    Role.allByForsyth get c.toLower map {
      Piece(Player.fromP1(c.isUpper), _)
    }

}
