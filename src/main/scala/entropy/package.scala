package strategygames

package object entropy {

  val P1 = strategygames.Player.P1
  val P2 = strategygames.Player.P2

  type Direction  = Pos => Option[Pos]
  type Directions = List[Direction]

  type PieceMap = Map[Pos, Piece]

  // one row or column, in board order, holding the colour of each square that has a counter
  type Line = Vector[Option[Role]]

  type PositionHash = Array[Byte]

}
