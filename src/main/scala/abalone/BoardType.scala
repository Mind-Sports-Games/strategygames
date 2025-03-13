package abalone

import abalone.util.geometry.Cell
import abalone.util.geometry.norm.{N6, Norm}

sealed abstract class BoardType(
                                 val width: Int,
                                 val height: Int,
                                 val norm: Norm = N6
                               ) {
  val key = s"${width}x${height}"
  //val validPos: List[Pos] = Pos.all

  final val cellList: List[Cell] = Range(0, height)
    .flatMap(y => Range(0, width)
      .filter(x => isCell(x, y))
      .map(x => new Cell(x, y))
    ).toList
  final val cellSet: Set[Cell] = cellList.toSet

  //
  // Cell
  final def isCell(a: Cell): Boolean = isCell(a.x, a.y)

  def isCell(x: Int, y: Int): Boolean = 0 <= x & x < width & 0 <= y & y < height

  //
  //
  override def toString = key
}

object BoardType {
  val all: List[BoardType] = List(Hex5, Hex6)
}

/** A Hexagon of side n fits in a square of side 2n - 1 */
sealed abstract class HexBoardType(val side: Int) extends BoardType(width = 2 * side - 1, height = 2 * side - 1) {
  val centre = new Cell(side - 1, side - 1)

  override val key = s"hex-${side}"

  override def isCell(x: Int, y: Int) = norm.dist(centre, x, y) < side
}

case object Hex5 extends HexBoardType(5)

case object Hex6 extends HexBoardType(6)