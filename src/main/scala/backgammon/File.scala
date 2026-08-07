package strategygames.backgammon

case class File private (val index: Int) extends AnyVal with Ordered[File] {

  @inline def -(that: File): Int           = index - that.index
  @inline override def compare(that: File) = this - that

  def offset(delta: Int): Option[File] =
    if (-File.allSize < delta && delta < File.allSize) File(index + delta)
    else None

  @inline def char: Char = (97 + index).toChar
  override def toString  = char.toString

  @inline def upperCaseChar: Char = (65 + index).toChar
  def toUpperCaseString           = upperCaseChar.toString

}

object File {

  def apply(index: Int): Option[File] =
    if (0 <= index && index < allSize) Some(new File(index))
    else None

  @inline def of(pos: Pos): File = {
    val rank: Int = if (pos.index >= allSize) 1 else 0
    new File(if (rank == 0) allSize - 1 - pos.index else pos.index - allSize)
  }

  def fromChar(ch: Char): Option[File] = apply(ch.toInt - 97)

  val A = new File(0)
  val B = new File(1)
  val C = new File(2)
  val D = new File(3)
  val E = new File(4)
  val F = new File(5)
  val G = new File(6)
  val H = new File(7)
  val I = new File(8)
  val J = new File(9)
  val K = new File(10)
  val L = new File(11)

  val all                     = List(A, B, C, D, E, F, G, H, I, J, K, L)
  val allReversed: List[File] = all.reverse
  val allSize: Int            = all.size

}
