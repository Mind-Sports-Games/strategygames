package chess

class Piece(val color: Color, val role: Role) {

  def is(c: Color)    = c == color
  def is(r: Role)     = r == role
  def isNot(c: Color) = c != color
  def isNot(r: Role)  = r != role

  def oneOf(rs: Set[Role]) = rs(role)

  override def toString = s"$color-$role".toLowerCase
}

case class ChessPiece(color: Color, role: ChessRole) extends Piece(color, role){

  def isMinor = oneOf(Set(Knight, Bishop))
  def isMajor = oneOf(Set(Queen, Rook))

  def forsyth: Char = if (color == White) role.forsythUpper else role.forsyth

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
      case Pawn => ChessPiece.pawnEyes(color, from, to)
      case LOAChecker  => (from onSameLine to) || (from onSameDiagonal to)
    }

  // movable positions assuming empty board
  def eyesMovable(from: Pos, to: Pos): Boolean =
    if (role == Pawn) ChessPiece.pawnEyes(color, from, to) || {
      (from ?| to) && {
        val dy = to.rank - from.rank
        if (color.white) (dy == 1 || (from.rank <= Rank.Second && dy == 2))
        else (dy == -1 || (from.rank >= Rank.Seventh && dy == -2))
      }
    }
    else eyes(from, to)

}

case class DraughtsPiece(color: Color, role: DraughtsRole) extends Piece(color, role){

  def isGhost = role == GhostCheckerMan || role == GhostCheckerKing

  def forsyth: Char = role.forsyth

  def ghostRole =
    if (role == CheckerMan) GhostCheckerMan 
    else if (role == CheckerKing) GhostCheckerKing
    else role

}

//object Piece

object ChessPiece {

  def fromChar(c: Char): Option[Piece] =
    Role.allByPgn get c.toUpper map {
      ChessPiece(Color.fromWhite(c.isUpper), _)
    }

  private def pawnEyes(color: Color, from: Pos, to: Pos) =
    (from xDist to) == 1 && (to.rank - from.rank) == {
      if (color.white) 1 else -1
    }
}

object DraughtsPiece {

  def fromChar(c: Char): Option[Piece] =
    Role.allByPdn get c.toUpper map {
      DraughtsPiece(Color.fromWhite(c.isUpper), _)
    }

}
