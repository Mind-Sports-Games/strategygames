import ornicar.scalalib

package object strategygames extends scalalib.Common with scalalib.OrnicarOption with scalalib.OrnicarBoolean {

  val P1 = Player.P1
  val P2 = Player.P2

  type PieceMap = Map[Pos, Piece]

  type PositionHash = Array[Byte]

  type MoveOrDrop = Either[Move, Drop]

}
