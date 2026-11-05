package strategygames.yonkoma
package format.pgn

import strategygames.yonkoma.format.Uci
import strategygames.ActionStrs

import scala.util.Try

object Binary {

  // writeMove only used in tests
  def writeMove(m: String)             = Try(Writer.ply(m))
  def writeMoves(ms: Iterable[String]) = Try(Writer.plies(ms))

  def writeActionStrs(ms: ActionStrs) = Try(Writer.actionStrs(ms))

  def readActionStrs(bs: List[Byte])          = Try(Reader actionStrs bs)
  def readActionStrs(bs: List[Byte], nb: Int) = Try(Reader.actionStrs(bs, nb))

  // As in go and entropy, the two high bits of an action's first byte give its type, so
  // further action types have room. A pass is that byte alone. A move is three bytes: the
  // type byte, the origin square index and the destination square index, each of which
  // runs from 0 to 143 on the 12x12 board and fits a byte.
  private object MoveType {
    val Pass = 0
    val Move = 1
  }

  private def right(i: Int, x: Int): Int = i & lengthMasks(x)
  private val lengthMasks                =
    Map(1 -> 0x01, 2 -> 0x03, 3 -> 0x07, 4 -> 0x0f, 5 -> 0x1f, 6 -> 0x3f, 7 -> 0x7f, 8 -> 0xff)

  private object Reader {

    // If changing this, consider changing other gamelogics and also lila game maxPlies
    private val maxPlies = 1000

    def actionStrs(bs: List[Byte]): ActionStrs          = actionStrs(bs, maxPlies)
    def actionStrs(bs: List[Byte], nb: Int): ActionStrs = toActionStrs(intPlies(bs map toInt, nb))

    // every action is a turn of its own
    def toActionStrs(plies: List[String]): ActionStrs = plies.map(Vector(_)).toVector

    def intPlies(bs: List[Int], pliesToGo: Int): List[String] =
      bs match {
        case _ if pliesToGo <= 0                                      => Nil
        case Nil                                                      => Nil
        case b1 :: rest if headerBit(b1) == MoveType.Pass             =>
          passUci :: intPlies(rest, pliesToGo - 1)
        case b1 :: b2 :: b3 :: rest if headerBit(b1) == MoveType.Move =>
          moveUci(b2, b3) :: intPlies(rest, pliesToGo - 1)
        case x                                                        => !!(x map showByte mkString ",")
      }

    // 2 movetype, 6 unused
    // ----
    // 8 pos (orig)
    // ----
    // 8 pos (dest)
    def moveUci(b2: Int, b3: Int): String = s"${posFromInt(b2)}${posFromInt(b3)}"

    val passUci = "pass"

    def posFromInt(b: Int): String = Pos(b).getOrElse(!!(s"no square ${b}")).key

    private def headerBit(i: Int) = i >> 6

    private def !!(msg: String) = throw new Exception("Binary reader failed: " + msg)
  }

  private object Writer {

    def ply(str: String): List[Byte] =
      (str match {
        case Uci.Move.moveR(orig, dest) => moveUci(orig, dest)
        case Uci.Pass.passR()           => passUci
        case _                          => sys.error(s"Invalid action to write: ${str}")
      }) map (_.toByte)

    def plies(strs: Iterable[String]): Array[Byte] =
      strs.toList.flatMap(ply).to(Array)

    // safe to flatten: every action is a turn of its own
    def actionStrs(strs: ActionStrs): Array[Byte] = plies(strs.flatten)

    def moveUci(orig: String, dest: String) = List(
      headerBit(MoveType.Move),
      right(Pos.fromKey(orig).get.index, 8),
      right(Pos.fromKey(dest).get.index, 8)
    )

    def passUci = List(headerBit(MoveType.Pass))

    private def headerBit(i: Int) = i << 6

  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String    = "%08d" format (b.toBinaryString.toInt)
}
