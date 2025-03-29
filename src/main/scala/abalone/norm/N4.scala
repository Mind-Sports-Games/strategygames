package strategygames.abalone.norm

object N4 extends Norm(radius = 1) {
  override def apply(x: Int, y: Int): Int = math.abs(x) + math.abs(y)

  override def apply(x: Double, y: Double): Double = math.abs(x) + math.abs(y)
}
