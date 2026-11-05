package strategygames.yonkoma

case class Rank private (val index: Int) extends AnyVal with Ordered[Rank] {
  @inline def -(that: Rank): Int           = index - that.index
  @inline override def compare(that: Rank) = this - that

  override def toString = (index + 1).toString
}

object Rank {
  def apply(index: Int): Option[Rank] =
    if (0 <= index && index < allSize) Some(new Rank(index))
    else None

  @inline def of(pos: Pos): Rank = new Rank(pos.index / File.allSize)

  val First    = new Rank(0)
  val Second   = new Rank(1)
  val Third    = new Rank(2)
  val Fourth   = new Rank(3)
  val Fifth    = new Rank(4)
  val Sixth    = new Rank(5)
  val Seventh  = new Rank(6)
  val Eighth   = new Rank(7)
  val Ninth    = new Rank(8)
  val Tenth    = new Rank(9)
  val Eleventh = new Rank(10)
  val Twelfth  = new Rank(11)

  // the largest board is 12x12; a smaller board uses the first ranks
  val all          = List(
    First,
    Second,
    Third,
    Fourth,
    Fifth,
    Sixth,
    Seventh,
    Eighth,
    Ninth,
    Tenth,
    Eleventh,
    Twelfth
  )
  val allSize: Int = all.size
}
