package strategygames.togyzkumalak
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
    val rank: Int = if (pos.index > 8) 1 else 0
    new File(pos.index + ((pos.index - allSize) * -2 - 1) * rank)
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

  val all                    = List(A, B, C, D, E, F, G, H, I)
  val allSize: Int           = all.size
  def allByWidth(width: Int) = all.take(width)
}
