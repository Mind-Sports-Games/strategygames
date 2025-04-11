package strategygames.abalone

import strategygames.abalone.norm.{N6, Norm}

sealed abstract class BoardType(
    val width: Int,
    val height: Int,
    val norm: Norm = N6
) {
  val key = s"${width}x${height}"
  // val validPos: List[Pos] = Pos.all

  final val posNb: Int = width * height

  final val cellList: List[Pos] = Range(0, height)
    .flatMap(y =>
      Range(0, width)
        .filter(x => isCell(x, y))
        .map(x => new Pos(x, y))
    )
    .toList
  final val cellSet: Set[Pos]   = cellList.toSet

  //
  // Cell
  final def isCell(a: Pos): Boolean = isCell(a.x, a.y)

  def isCell(x: Int, y: Int): Boolean = 0 <= x && x < width && 0 <= y && y < height

  //
  //
  override def toString = key
}

object BoardType {
  val all: List[BoardType] = List(Hex5, Hex6)
}

/** A Hexagon of side n fits in a square of side 2n - 1 */
sealed abstract class HexBoardType(val side: Int)
    extends BoardType(width = 2 * side - 1, height = 2 * side - 1) {
  override val key = s"hex-${side}"

  override def isCell(x: Int, y: Int) = norm.dist(side - 1, side - 1, x, y) < side
}

case object Hex5 extends HexBoardType(5)

case object Hex6 extends HexBoardType(6)
