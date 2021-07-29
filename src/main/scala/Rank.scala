package chess

case class Rank private (val index: Int) extends AnyVal with Ordered[Rank] {
  @inline def -(that: Rank): Int           = index - that.index
  @inline override def compare(that: Rank) = this - that

  def offset(delta: Int): Option[Rank] =
    if (-10 < delta && delta < 10) Rank(index + delta)
    else None

  override def toString = (index+1).toString()
}

object Rank {
  def apply(index: Int): Option[Rank] =
    if (0 <= index && index < 10) Some(new Rank(index))
    else None

  @inline def of(pos: Pos): Rank = new Rank(pos.index >> 3)

  val First   = new Rank(0)
  val Second  = new Rank(1)
  val Third   = new Rank(2)
  val Fourth  = new Rank(3)
  val Fifth   = new Rank(4)
  val Sixth   = new Rank(5)
  val Seventh = new Rank(6)
  val Eighth  = new Rank(7)
  val Ninth   = new Rank(8)
  val Tenth   = new Rank(9)

  def all = List(First, Second, Third, Fourth, Fifth, Sixth, Seventh, Eighth, Ninth, Tenth)
  def allForBoard(max: Int): List[Rank] = all.filter(_.index < max)

  def allReversed: List[Rank] = all.reverse
  def allForBoardReversed(max: Int): List[Rank] = allForBoard(max).reverse
}
