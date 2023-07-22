package strategygames.go
package format.pgn

import strategygames.go.format.Uci

import scala.util.Try

object Binary {

  // writeMove only used in tests
  def writeMove(m: String)             = Try(Writer.move(m))
  def writeMoves(ms: Iterable[String]) = Try(Writer.moves(ms))

  def readMoves(bs: List[Byte])          = Try(Reader moves bs)
  def readMoves(bs: List[Byte], nb: Int) = Try(Reader.moves(bs, nb))

  private object MoveType {
    val Pass = 0
    val Drop = 1
  }

  private def right(i: Int, x: Int): Int = i & lengthMasks(x)
  private val lengthMasks                =
    Map(1 -> 0x01, 2 -> 0x03, 3 -> 0x07, 4 -> 0x0f, 5 -> 0x1f, 6 -> 0x3f, 7 -> 0x7f, 8 -> 0xff)

  private object Reader {

    private val maxPlies = 600

    def moves(bs: List[Byte]): List[String]          = moves(bs, maxPlies)
    def moves(bs: List[Byte], nb: Int): List[String] = intMoves(bs map toInt, nb)

    def intMoves(bs: List[Int], pliesToGo: Int): List[String] =
      bs match {
        case _ if pliesToGo <= 0                                  => Nil
        case Nil                                                  => Nil
        case (b1 :: b2 :: rest) if headerBit(b1) == MoveType.Drop =>
          dropUci(b1, b2) :: intMoves(rest, pliesToGo - 1)
        case (b1 :: rest) if headerBit(b1) == MoveType.Pass       =>
          passUci :: intMoves(rest, pliesToGo - 1)
        case x                                                    => !!(x map showByte mkString ",")
      }

    // 2 movetype
    // 6 pos (dest) first 6 bits (but if only needing up to 361 this will essentially leave 5 bits free in the middle and only use the least significant bit)
    // ----
    // 8 pos (dest) last 8 bits. Total 2**14 = 16384
    def dropUci(b1: Int, b2: Int): String =
      s"s@${posFromInt(b1, b2)}"

    val passUci                              = "pass"
    def posFromInt(b1: Int, b2: Int): String = Pos((right(b1, 6) << 8) + b2).get.toString()

    private def headerBit(i: Int) = i >> 6

    private def !!(msg: String) = throw new Exception("Binary reader failed: " + msg)
  }

  private object Writer {

    def move(str: String): List[Byte] =
      (str match {
        case Uci.Drop.dropR(_, dst) => dropUci(dst)
        case Uci.Pass.passR()       => passUci
        case _                      => sys.error(s"Invalid move to write: ${str}")
      }) map (_.toByte)

    def moves(strs: Iterable[String]): Array[Byte] =
      strs.toList.flatMap(move).to(Array)

    def passUci = List(headerBit(MoveType.Pass))

    def dropUci(dst: String) = List(
      (headerBit(MoveType.Drop)) + (Pos.fromKey(dst).get.index >> 8),
      right(Pos.fromKey(dst).get.index, 8)
    )

    private def headerBit(i: Int) = i << 6

  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String    = "%08d" format (b.toBinaryString.toInt)
}
