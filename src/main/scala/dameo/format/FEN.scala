package strategygames.dameo.format

import strategygames.Player
import strategygames.dameo.{ Piece, PieceMap, Pos, Role }
/*

Dameo FEN:
[colour to move]:W[white piece coords]:B[black piece coords]:Halfmoveclock:Fullmoves

coords for men are a1,b1,c1
other roles (kings, ghostman, ghostking, activeman, activeking) are added to the coord with a period,
e.g. a1.k,b1.g,b2.p I assume ghosts need to be part of the FEN since we want to treat (partial)
moves with ghosts in them as proper moves w.r.t. the frontend.

*/

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  def splitted: Array[String] = value.split(':')

  def player: Option[Player] =
    splitted.lift(0) flatMap (_.headOption) flatMap Player.apply

  def initial = value == Forsyth.initial.value

  def player1: Player = Player.apply(splitted(1).head).get
  def pieceStr1: String = splitted(1).tail
  def pieces1: Array[(Pos, Piece)] = pieceStr1
    .split(',').filter(_!="")
    .map(parsePiece(player1))

  def player2: Player = Player.apply(splitted(2).head).get
  def pieceStr2: String = splitted(2).tail
  def pieces2: Array[(Pos, Piece)] = pieceStr2
    .split(',').filter(_!="")
    .map(parsePiece(player2))

  def pieces: PieceMap = (pieces1 ++ pieces2).toMap

  def halfMoveClock: Option[Int] = intFromFen(FEN.halfMoveIndex)
  def fullMove: Option[Int] = intFromFen(FEN.fullMoveIndex)

  private def parsePiece(player: Player)(pStr: String): (Pos, Piece) = {
    def pStrA: Array[String] = pStr.split('.')
    def role: Role = pStrA.lift(1) match {
      case Some(roleStr) => Role.forsyth(roleStr.head).get
      case _             => Role.defaultRole
    }
    (Pos.allKeys(pStrA(0)), Piece(player, role))
  }

  private def intFromFen(index: Int): Option[Int] =
    value.split(':').lift(index).flatMap(_.tail.toIntOption)
}

object FEN {

  def halfMoveIndex: Int = 3
  def fullMoveIndex: Int = 4

}
