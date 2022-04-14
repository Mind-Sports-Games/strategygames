package strategygames.mancala

import strategygames.Player

case class Rank private (val index: Int) extends AnyVal with Ordered[Rank] {
  @inline def -(that: Rank): Int           = index - that.index
  @inline override def compare(that: Rank) = this - that

  def offset(delta: Int): Option[Rank] =
    if (-Rank.all.size < delta && delta < Rank.all.size) Rank(index + delta)
    else None

  @inline def char: Char = if (index < 9) (49 + index).toChar else 48.toChar//0
  override def toString  = (index+1).toString
}

object Rank {
  def apply(index: Int): Option[Rank] =
    if (0 <= index && index < all.size) Some(new Rank(index))
    else None

  @inline def of(pos: Pos): Rank = new Rank(pos.index/File.all.size)

  def fromChar(ch: Char): Option[Rank] = apply(if (ch.toInt == 48) 9 else ch.toInt - 49)

  val First   = new Rank(0)
  val Second  = new Rank(1)
  // val Third   = new Rank(2)
  // val Fourth  = new Rank(3)
  // val Fifth   = new Rank(4)
  // val Sixth   = new Rank(5)
  // val Seventh = new Rank(6)
  // val Eighth  = new Rank(7)
  // val Ninth   = new Rank(8)
  // val Tenth   = new Rank(9)

  //val all = List(First, Second, Third, Fourth, Fifth, Sixth, Seventh, Eighth, Ninth, Tenth)
  val all = List(First, Second)
  val allReversed: List[Rank] = all.reverse

}
