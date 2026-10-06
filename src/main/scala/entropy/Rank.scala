package strategygames.entropy

case class Rank private (val index: Int) extends AnyVal with Ordered[Rank] {
  @inline def -(that: Rank): Int           = index - that.index
  @inline override def compare(that: Rank) = this - that

  def offset(delta: Int): Option[Rank] =
    if (-Rank.allSize < delta && delta < Rank.allSize) Rank(index + delta)
    else None

  @inline def char: Char           = (49 + index).toChar
  def sgfChar(numRanks: Int): Char = (97 + (numRanks - 1 - index)).toChar
  override def toString            = (index + 1).toString
}

object Rank {
  def apply(index: Int): Option[Rank] =
    if (0 <= index && index < allSize) Some(new Rank(index))
    else None

  @inline def of(pos: Pos): Rank = new Rank(pos.index / File.allSize)

  def fromChar(ch: Char): Option[Rank] = apply(ch.toInt - 49)

  val First   = new Rank(0)
  val Second  = new Rank(1)
  val Third   = new Rank(2)
  val Fourth  = new Rank(3)
  val Fifth   = new Rank(4)
  val Sixth   = new Rank(5)
  val Seventh = new Rank(6)

  val all                     = List(First, Second, Third, Fourth, Fifth, Sixth, Seventh)
  val allReversed: List[Rank] = all.reverse
  val allSize: Int            = all.size
}
