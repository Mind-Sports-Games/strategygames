package abalone.util.geometry.norm

object N8 extends Norm(radius = 1) {
  override def apply(x: Int, y: Int): Int = math.max(math.abs(x), math.abs(y))
}
