package strategygames

import ornicar.scalalib
import strategygames.abalone.geometry.Cell

package object abalone extends scalalib.Common with scalalib.OrnicarOption with scalalib.OrnicarBoolean {
  val P1 = strategygames.Player.P1
  val P2 = strategygames.Player.P2

  type Direction = Pos => Option[Pos]
  type Directions = List[Direction]

  type PieceMap = Map[Cell, Piece]

  type PositionHash = Array[Byte]
}