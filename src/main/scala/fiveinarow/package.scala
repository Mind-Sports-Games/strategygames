package strategygames

package object fiveinarow {

  val P1 = strategygames.Player.P1
  val P2 = strategygames.Player.P2

  type PieceMap = Map[Pos, Piece]

  type PositionHash = Array[Byte]

}
