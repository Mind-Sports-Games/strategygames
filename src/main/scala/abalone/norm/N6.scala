package strategygames.abalone.norm

object N6 extends Norm(radius = 1, is3 = true) {
  override def apply(x: Int, y: Int): Int = {
    if (x * y < 0) math.abs(x) + math.abs(y)
    else math.max(math.abs(x), math.abs(y))
  }

  override def apply(x: Double, y: Double): Double = {
    if (x * y < 0) math.abs(x) + math.abs(y)
    else math.max(math.abs(x), math.abs(y))
  }
}
