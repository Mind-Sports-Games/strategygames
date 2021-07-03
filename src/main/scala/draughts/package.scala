import ornicar.scalalib

import scala.util.Try

package object draughts extends scalalib.Common with scalalib.OrnicarOption with scalalib.OrnicarBoolean {

  val White = Color.White
  val Black = Color.Black

  type Direction  = (Int, PosMotion => Option[PosMotion])
  type Directions = List[Direction]

  type PieceMap = Map[Pos, Piece]

  type PositionHash = Array[Byte]

  //object implicitFailures {
  //  implicit def stringToFailures(str: String): Failures = scalaz.NonEmptyList(str)
  //}

  def parseIntOption(str: String): Option[Int] =
    Try(Integer.parseInt(str)).toOption

  //private[draughts] val logger = draughtsLog("draughts")

}
