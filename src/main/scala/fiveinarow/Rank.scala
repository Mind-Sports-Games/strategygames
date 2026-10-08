package strategygames.fiveinarow

case class Rank private (val index: Int) extends AnyVal with Ordered[Rank] {
  @inline def -(that: Rank): Int           = index - that.index
  @inline override def compare(that: Rank) = this - that

  def offset(delta: Int): Option[Rank] =
    if (-Rank.allSize < delta && delta < Rank.allSize) Rank(index + delta)
    else None

  override def toString = (index + 1).toString
}

object Rank {
  def apply(index: Int): Option[Rank] =
    if (0 <= index && index < allSize) Some(new Rank(index))
    else None

  @inline def of(pos: Pos): Rank = new Rank(pos.index / File.allSize)

  def fromString(s: String): Option[Rank] = s.toIntOption.flatMap(i => apply(i - 1))

  val all: List[Rank]         = (0 until 15).map(new Rank(_)).toList
  val allReversed: List[Rank] = all.reverse
  val allSize: Int            = all.size

  val First = new Rank(0)
}
