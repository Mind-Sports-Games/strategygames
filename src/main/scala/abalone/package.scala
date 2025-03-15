package strategygames

import ornicar.scalalib

package object abalone extends scalalib.Common with scalalib.OrnicarOption with scalalib.OrnicarBoolean {
  val P1 = strategygames.Player.P1
  val P2 = strategygames.Player.P2

  type Direction = Pos => Option[Pos]
  type Directions = List[Direction]

  @deprecated("Alex", since="1.5.5") type PieceMap = Map[Pos, Piece]

  type PositionHash = Array[Byte]
}