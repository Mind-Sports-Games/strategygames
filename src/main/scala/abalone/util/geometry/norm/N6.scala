package abalone.util.geometry.norm

object N6 extends Norm(radius = 1) {
  override def apply(x: Int, y: Int): Int = {
    if (x * y < 0) math.abs(x) + math.abs(y)
    else math.max(math.abs(x), math.abs(y))
  }
}
