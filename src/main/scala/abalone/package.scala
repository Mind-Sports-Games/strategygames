package strategygames

import ornicar.scalalib

package object abalone extends scalalib.Common with scalalib.OrnicarOption with scalalib.OrnicarBoolean {
  val P1 = strategygames.Player.P1
  val P2 = strategygames.Player.P2

  type PieceMap = Map[Pos, Piece]

  type PositionHash = Array[Byte]
}
