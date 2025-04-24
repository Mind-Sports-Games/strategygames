package strategygames.abalone.norm

object N12 extends Norm(radius = 2, is3 = true) {
  override def apply(x: Int, y: Int): Int = {
    if (x * y <= 0) math.max(math.abs(x), math.abs(y))
    if ((y - x) * x >= 0) math.max(math.abs(y - x), math.abs(x))
    math.max(math.abs(y), math.abs(x - y))
  }

  override def apply(x: Double, y: Double): Double = {
    if (x * y <= 0) math.max(math.abs(x), math.abs(y))
    if ((y - x) * x >= 0) math.max(math.abs(y - x), math.abs(x))
    math.max(math.abs(y), math.abs(x - y))
  }
}
