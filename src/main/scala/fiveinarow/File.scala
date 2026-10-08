package strategygames.fiveinarow

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

  @inline def of(pos: Pos): File = new File(pos.index % allSize)

  def fromChar(ch: Char): Option[File] = apply(ch.toInt - 97)

  val all: List[File] = (0 until 15).map(new File(_)).toList
  val allSize: Int    = all.size
}
