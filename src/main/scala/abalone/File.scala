//package strategygames.abalone
//
//case class File private(index: Int) extends AnyVal with Ordered[File] {
//  @inline def -(that: File): Int = index - that.index
//
//  @inline override def compare(that: File) = this - that
//
//  def offset(delta: Int): Option[File] =
//    if (-File.all.size < delta && delta < File.all.size) File(index + delta)
//    else None
//
//  @inline def char: Char = (97 + index).toChar
//
//  override def toString = char.toString
//
//  @inline def upperCaseChar: Char = (65 + index).toChar
//
//  def toUpperCaseString = upperCaseChar.toString
//}
//
///** Column (as if it were an index in a 1D array) */
//object File {
//  def apply(index: Int): Option[File] =
//    if (0 <= index && index < all.size) Some(new File(index))
//    else None
//
//  @inline def of(pos: Pos): File = {
//    new File(pos.index % all.size)
//  }
//
//  def fromChar(ch: Char): Option[File] = apply(ch.toInt - 97)
//
//  val A = new File(0)
//  val B = new File(1)
//  val C = new File(2)
//  val D = new File(3)
//  val E = new File(4)
//  val F = new File(5)
//  val G = new File(6)
//  val H = new File(7)
//  val I = new File(8)
//
//  val all = List(A, B, C, D, E, F, G, H, I)
//  val allReversed: List[File] = all.reverse
//}