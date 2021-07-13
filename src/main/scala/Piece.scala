package strategygames

sealed class Piece(val color: Color, val role: Role) {

  def is(c: Color)    = c == color
  def is(r: Role)     = r == role
  def isNot(c: Color) = c != color
  def isNot(r: Role)  = r != role

  def oneOf(rs: Set[Role]) = rs(role)

  def forsyth: Char

  override def toString = s"$color-$role".toLowerCase

}

object Piece {

  final case class Chess(p: chess.Piece) extends Piece(
    Color.Chess(p.color),
    Role.Chess(p.role)
  ) {

    def forsyth: Char = p.forsyth

  }

  final case class Draughts(p: draughts.Piece) extends Piece(
    Color.Draughts(p.color),
    Role.Draughts(p.role)
  ){

    def forsyth: Char = p.forsyth

  }

}
