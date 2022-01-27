package strategygames.chess

import strategygames.Player

case class Piece(player: Player, role: Role) {

  def is(c: Player)    = c == player
  def is(r: Role)     = r == role
  def isNot(c: Player) = c != player
  def isNot(r: Role)  = r != role

  def oneOf(rs: Set[Role]) = rs(role)

  def isMinor = oneOf(Set(Knight, Bishop))
  def isMajor = oneOf(Set(Queen, Rook))

  def forsyth: Char = if (player == P1) role.forsythUpper else role.forsyth

  // attackable positions assuming empty board
  def eyes(from: Pos, to: Pos): Boolean =
    role match {
      case King   => from touches to
      case Queen  => (from onSameLine to) || (from onSameDiagonal to)
      case Rook   => from onSameLine to
      case Bishop => from onSameDiagonal to
      case Knight =>
        val xd = from xDist to
        val yd = from yDist to
        (xd == 1 && yd == 2) || (xd == 2 && yd == 1)
      case Pawn       => Piece.pawnEyes(player, from, to)
      case LOAChecker => (from onSameLine to) || (from onSameDiagonal to)
    }

  // movable positions assuming empty board
  def eyesMovable(from: Pos, to: Pos): Boolean =
    if (role == Pawn) Piece.pawnEyes(player, from, to) || {
      (from ?| to) && {
        val dy = to.rank - from.rank
        if (player.p1) (dy == 1 || (from.rank <= Rank.Second && dy == 2))
        else (dy == -1 || (from.rank >= Rank.Seventh && dy == -2))
      }
    }
    else eyes(from, to)

  override def toString = s"$player-$role".toLowerCase
}

object Piece {

  def fromChar(c: Char): Option[Piece] =
    Role.allByPgn get c.toUpper map {
      Piece(Player.fromP1(c.isUpper), _)
    }

  private def pawnEyes(player: Player, from: Pos, to: Pos) =
    (from xDist to) == 1 && (to.rank - from.rank) == {
      if (player.p1) 1 else -1
    }
}
