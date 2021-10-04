package strategygames
import ornicar.scalalib

import scala.util.Try

package object fairysf extends scalalib.Common with scalalib.OrnicarOption with scalalib.OrnicarBoolean {

  val White = strategygames.Color.White
  val Black = strategygames.Color.Black

  type Direction  = (Int, PosMotion => Option[PosMotion])
  type Directions = List[Direction]

  type PieceMap = Map[Pos, Piece]

  type PositionHash = Array[Byte]

  def parseIntOption(str: String): Option[Int] =
    Try(Integer.parseInt(str)).toOption

}
