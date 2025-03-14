package abalone.util.geometry.norm

import abalone.util.geometry.Cell

abstract class Norm(val radius: Int, is3: Boolean = false) {
  final def apply(a: Cell): Int = this (a.x, a.y)

  def apply(x: Int, y: Int): Int

  final def apply(x: (Double, Double)): Double = this (x._1, x._2)

  def apply(x: Double, y: Double): Double

  //
  // Distance
  final def dist(a: Cell, b: Cell): Int = dist(a, b.x, b.y)

  final def dist(a: Cell, x: Int, y: Int): Int = dist(a.x, a.y, x, y)

  final def dist(x: Int, y: Int, a: Cell): Int = dist(x, y, a.x, a.y)

  final def dist(x: Int, y: Int, z: Int, t: Int): Int = this (z - x, t - y)

  final def dist(a: (Double, Double), b: (Double, Double)): Double = dist(a, b._1, b._2)

  final def dist(a: (Double, Double), x: Double, y: Double): Double = dist(a._1, a._2, x, y)

  final def dist(x: Double, y: Double, a: (Double, Double)): Double = dist(x, y, a._2, a._2)

  final def dist(x: Double, y: Double, z: Double, t: Double): Double = this (z - x, t - y)

  //
  // Neighbourhood
  val neighVectors: Set[Cell] = (0 to radius)
    .flatMap(y => (0 to radius)
      .filter(x => this (x, y) == 1)
      .map(x => new Cell(x, y))
    )
    .toSet

  final def getNeigh(a: Cell): Set[(Cell, Cell)] = neighVectors.map(vect => (vect, a + vect))

  //
  // Rotation
  val unitDeg = 360d / neighVectors.size

  final def getNext(a: Cell): Cell = getRotatedKeepNorm(a, unitDeg)

  final def getPrev(a: Cell): Cell = getRotatedKeepNorm(a, -unitDeg)

  final def getRotatedKeepNorm(a: Cell, deg: Double, rot: (Cell, Double) => (Double, Double) = (b, d) => getRotated(b, d)): Cell = {
    var p = rot(a, deg)

    var n = this (p)
    if (n > 0) {
      n = this (a) / n
      p = (p._1 * n, p._2 * n)
    }

    Cell.fromPoint(p)
  }

  private final def getRotated(a: Cell, deg: Double): (Double, Double) =
    if (is3) Cell.getRotated(a.vectTo3, deg)
    else Cell.getRotated(a, deg)
}