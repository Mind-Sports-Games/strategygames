package strategygames

package object chess {

  val P1 = strategygames.Player.P1
  val P2 = strategygames.Player.P2

  type Direction  = Pos => Option[Pos]
  type Directions = List[Direction]

  type PieceMap = Map[Pos, Piece]

  type PositionHash = Array[Byte]

}
