package strategygames
import ornicar.scalalib

package object abalone extends scalalib.Common with scalalib.OrnicarOption with scalalib.OrnicarBoolean {

  // @TODO: check this was the correct way to handle the fact p1 is black and p2 is white
  val P1 = strategygames.Player.P2
  val P2 = strategygames.Player.P1

  type Direction  = Pos => Option[Pos]
  type Directions = List[Direction]

  type PieceMap = Map[Pos, Piece]

  type PositionHash = Array[Byte]

}
