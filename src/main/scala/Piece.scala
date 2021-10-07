package strategygames

abstract sealed class Piece(val color: Color, val role: Role) {

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
    p.color,
    Role.ChessRole(p.role)
  ) {

    def forsyth: Char = p.forsyth

  }

  final case class Draughts(p: draughts.Piece) extends Piece(
    p.color,
    Role.DraughtsRole(p.role)
  ){

    def forsyth: Char = p.forsyth

  }

  final case class FairySF(p: fairysf.Piece) extends Piece(
    p.color,
    Role.FairySFRole(p.role)
  ){

    def forsyth: Char = p.forsyth

  }

  def apply(lib: GameLogic, color: Color, role: Role): Piece = (lib, role) match {
    case (GameLogic.Draughts(), Role.DraughtsRole(role)) => Draughts(draughts.Piece(color, role))
    case (GameLogic.Chess(), Role.ChessRole(role))       => Chess(chess.Piece(color, role))
    case (GameLogic.FairySF(), Role.FairySFRole(role))   => FairySF(fairysf.Piece(color, role))
    case _ => sys.error("Mismatched gamelogic types 2")
  }

  def fromChar(lib: GameLogic, c: Char): Option[Piece] = lib match {
    case (GameLogic.Draughts()) => draughts.Piece.fromChar(c).map(Draughts)
    case (GameLogic.Chess())    => chess.Piece.fromChar(c).map(Chess)
    case (GameLogic.FairySF())  => fairysf.Piece.fromChar(c).map(FairySF)
  }

  def chessPieceMap(pieceMap: PieceMap): chess.PieceMap = pieceMap.map{
    case(Pos.Chess(pos), Chess(piece)) => (pos, piece)
  }

  def draughtsPieceMap(pieceMap: PieceMap): draughts.PieceMap = pieceMap.map{
    case(Pos.Draughts(pos), Draughts(piece)) => (pos, piece)
  }

  def fairySFPieceMap(pieceMap: PieceMap): fairysf.PieceMap = pieceMap.map{
    case(Pos.FairySF(pos), FairySF(piece)) => (pos, piece)
  }
}
