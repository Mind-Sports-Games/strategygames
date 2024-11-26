package strategygames.abalone
package format.pgn

import strategygames.ActionStrs
import strategygames.abalone.format.Uci

import scala.util.Try

object Binary {

  // writeMove only used in tests for chess/draughts
  // def writeMove(m: String)             = Try(Writer.ply(gf, m))
  def writeMoves(ms: Iterable[String]) = Try(Writer.plies(ms))

  def writeActionStrs(ms: ActionStrs) = Try(Writer.actionStrs(ms))

  def readActionStrs(bs: List[Byte])          = Try(Reader actionStrs bs)
  def readActionStrs(bs: List[Byte], nb: Int) = Try(Reader.actionStrs(bs, nb))

  private object Reader {

    private val maxPlies = 600

    def actionStrs(bs: List[Byte]): ActionStrs          = actionStrs(bs, maxPlies)
    def actionStrs(bs: List[Byte], nb: Int): ActionStrs = toActionStrs(intPlies(bs map toInt, nb))

    def toActionStrs(plies: List[String]): ActionStrs = plies.map(List(_))

    def intPlies(bs: List[Int], pliesToGo: Int): List[String] =
      bs match {
        case _ if pliesToGo <= 0 => Nil
        case Nil                 => Nil
        case (b1 :: b2 :: rest)  =>
          moveUci(b1, b2) :: intPlies(rest, pliesToGo - 1)
        case x                   => !!(x map showByte mkString ",")
      }

    // 1 freebit (0)
    // 7 pos (from)
    // ----
    // 1 freebit (0)
    // 7 pos (dest)
    def moveUci(b1: Int, b2: Int): String =
      s"${posFromInt(b1)}${posFromInt(b2)}"

    def posFromInt(b: Int): String = Pos(right(b, 7)).get.toString()

    private def right(i: Int, x: Int): Int = i & lengthMasks(x)
    private val lengthMasks                =
      Map(1 -> 0x01, 2 -> 0x03, 3 -> 0x07, 4 -> 0x0f, 5 -> 0x1f, 6 -> 0x3f, 7 -> 0x7f, 8 -> 0xff)
    private def !!(msg: String)            = throw new Exception("Binary reader failed: " + msg)
  }

  private object Writer {

    def ply(str: String): List[Byte] =
      (str match {
        case Uci.Move.moveR(src, dst) => moveUci(src, dst)
        case _                        => sys.error(s"Invalid move to write: ${str}")
      }) map (_.toByte)

    def plies(strs: Iterable[String]): Array[Byte] =
      strs.toList.flatMap(ply).to(Array)

    // can flatten because this GameLogic doesn't support multimove
    def actionStrs(strs: ActionStrs): Array[Byte] = plies(strs.flatten)

    def moveUci(src: String, dst: String) = List(
      Pos.fromKey(src).get.index,
      Pos.fromKey(dst).get.index
    )

  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String    = "%08d" format (b.toBinaryString.toInt)
}
