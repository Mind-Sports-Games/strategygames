package strategygames.abalone.norm

import strategygames.abalone.Pos

abstract class Norm(val radius: Int, is3: Boolean = false) {
  final def apply(a: Pos): Int = this(a.x, a.y)

  def apply(x: Int, y: Int): Int

  final def apply(x: (Double, Double)): Double = this(x._1, x._2)

  def apply(x: Double, y: Double): Double

  //
  // Distance
  final def dist(a: Pos, b: Pos): Int = dist(a, b.x, b.y)

  final def dist(a: Pos, x: Int, y: Int): Int = dist(a.x, a.y, x, y)

  final def dist(x: Int, y: Int, a: Pos): Int = dist(x, y, a.x, a.y)

  final def dist(x: Int, y: Int, z: Int, t: Int): Int = this(z - x, t - y)

  final def dist(a: (Double, Double), b: (Double, Double)): Double = dist(a, b._1, b._2)

  final def dist(a: (Double, Double), x: Double, y: Double): Double = dist(a._1, a._2, x, y)

  final def dist(x: Double, y: Double, a: (Double, Double)): Double = dist(x, y, a._2, a._2)

  final def dist(x: Double, y: Double, z: Double, t: Double): Double = this(z - x, t - y)

  //
  // Neighbourhood
  val neighVectors: Set[Pos] = (-radius to radius)
    .flatMap(y =>
      (-radius to radius)
        .filter(x => this(x, y) == 1)
        .map(x => new Pos(x, y))
    )
    .toSet

  final def getNeigh(a: Pos): Set[(Pos, Pos)] = neighVectors.map(vect => (vect, a + vect))

  //
  // Products
  final def scal(a: Pos, b: Pos): Double = {
    if (is3) a.scal3(b)
    else a.scal(b)
  }

  final def cross(a: Pos, b: Pos): Double = {
    if (is3) a.cross3(b)
    else a.cross(b)
  }

  //
  // Rotation
  val unitDeg = 360d / neighVectors.size

  final def getNext(a: Pos): Pos = getRotatedKeepNorm(a, unitDeg)

  final def getPrev(a: Pos): Pos = getRotatedKeepNorm(a, -unitDeg)

  final def getRotatedKeepNorm(
      a: Pos,
      deg: Double,
      rot: (Pos, Double) => (Double, Double) = (b, d) => getRotated(b, d)
  ): Pos = {
    var p = rot(a, deg)

    var n = this(p)
    if (n > 0) {
      n = this(a) / n
      p = (p._1 * n, p._2 * n)
    }

    Pos.fromPoint(p)
  }

  final private def getRotated(a: Pos, deg: Double): (Double, Double) =
    if (is3) Pos.getRotated(a.vectTo3, deg)
    else Pos.getRotated(a, deg)
}
