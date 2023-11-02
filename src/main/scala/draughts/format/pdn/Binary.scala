package strategygames.draughts
package format.pdn

import scala.util.Try
import strategygames.ActionStrs

object Binary {

  def writeMove(m: String)             = Try(Writer ply m)
  def writeMoves(ms: Iterable[String]) = Try(Writer plies ms)

  def writeActionStrs(as: ActionStrs) = Try(Writer actionStrs as)

  def readActionStrs(bs: List[Byte])          = Try(Reader actionStrs bs)
  def readActionStrs(bs: List[Byte], nb: Int) = Try(Reader.actionStrs(bs, nb))

  private object MoveType {
    val IsMove    = 0
    val IsCapture = 1
    val Delimiter = 3
  }

  private object Delimiter {
    val str = ""
    val int = 255
  }

  private object Reader {

    private val maxPlies = 600

    def actionStrs(bs: List[Byte]): ActionStrs          = actionStrs(bs, maxPlies)
    def actionStrs(bs: List[Byte], nb: Int): ActionStrs = toActionStrs(intPlies(bs map toInt, nb, "x00"))

    def toActionStrs(plies: List[String]): ActionStrs =
      if (plies.contains(Delimiter.str)) unflatten(plies)
      else plies.map(List(_))

    def unflatten(plies: List[String]): List[List[String]] =
      if (plies.size == 0) List()
      else plies.takeWhile(_ != Delimiter.str) :: unflatten(plies.dropWhile(_ != Delimiter.str).drop(1))

    private def intPlies(bs: List[Int], pliesToGo: Int, lastUci: String): List[String] = bs match {
      case _ if pliesToGo < 0                                     => Nil
      case Nil                                                    => Nil
      case b1 :: rest if moveType(b1) == MoveType.Delimiter       =>
        Delimiter.str :: intPlies(rest, pliesToGo, "x00")
      case b1 :: b2 :: rest if moveType(b1) == MoveType.IsMove    =>
        if (pliesToGo == 0)
          Nil
        else
          moveUci(b1, b2) :: intPlies(rest, pliesToGo - 1, "x00")
      case b1 :: b2 :: rest if moveType(b1) == MoveType.IsCapture =>
        val newUci = captureUci(b1, b2)
        if (lastUci.endsWith("x" + newUci.substring(0, newUci.indexOf('x'))))
          newUci :: intPlies(rest, pliesToGo, newUci)
        else if (pliesToGo == 0)
          Nil
        else
          newUci :: intPlies(rest, pliesToGo - 1, newUci)
      case x                                                      => !!(x map showByte mkString ",")
    }

    // 255 => 11111111 => marker for end of turn. This makes movetype == 3 => Delimiter

    // 2 movetype
    // 6 srcPos
    // ----
    // 2 NOTHING
    // 6 dstPos
    def moveUci(b1: Int, b2: Int): String    = s"${right(b1, 6)}-${right(b2, 6)}"
    def captureUci(b1: Int, b2: Int): String = s"${right(b1, 6)}x${right(b2, 6)}"

    private def moveType(i: Int) = i >> 6

    private def right(i: Int, x: Int): Int = i & lengthMasks(x)
    private val lengthMasks                =
      Map(1 -> 0x01, 2 -> 0x03, 3 -> 0x07, 4 -> 0x0f, 5 -> 0x1f, 6 -> 0x3f, 7 -> 0x7f, 8 -> 0xff)
    private def !!(msg: String)            = throw new Exception("Binary reader failed: " + msg)

    // private def cut(i: Int, from: Int, to: Int): Int = right(i, from) >> to
    // private def bitAt(i: Int, p: Int): Boolean = cut(i, p, p - 1) != 0

  }

  private object Writer {

    def ply(str: String): List[Byte] = (str match {
      case Delimiter.str         => List(Delimiter.int)
      case MoveUciR(src, dst)    => moveUci(src, dst)
      case CaptureUciR(src, dst) => captureUci(src, dst)
      case _                     =>
        // TODO: log?
        // draughtsLog("ERROR: Binary").info(s"Cannot encode $str")
        Nil
    }) map (_.toByte)

    def plies(strs: Iterable[String]): Array[Byte] = strs.flatMap(ply).to(Array)

    def actionStrs(strs: ActionStrs): Array[Byte] =
      if (strs.size == 0 || strs.map(_.size).max == 1) plies(strs.flatten)
      else plies(strs.toList.map(_.toList :+ "").flatten)

    def moveUci(src: String, dst: String) = List(
      (MoveType.IsMove << 6) + src.toInt,
      dst.toInt
    )

    def captureUci(src: String, dst: String) = List(
      (MoveType.IsCapture << 6) + src.toInt,
      dst.toInt
    )

    val fieldR      = "(\\d+)"
    val MoveUciR    = s"$fieldR-$fieldR$$".r
    val CaptureUciR = s"${fieldR}x$fieldR$$".r

  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String    = "%08d" format (b.toBinaryString.toInt)

}
