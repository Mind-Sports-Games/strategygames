import ornicar.scalalib

package object strategygames extends scalalib.Common with scalalib.OrnicarOption with scalalib.OrnicarBoolean {

  val White = Color.White
  val Black = Color.Black

  type PieceMap = Map[Pos, Piece]

  type PositionHash = Array[Byte]

  type MoveOrDrop = Either[Move, chess.Drop]

}
