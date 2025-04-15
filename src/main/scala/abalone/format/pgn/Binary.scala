package strategygames.abalone.format.pgn

import strategygames.ActionStrs
import strategygames.abalone.Pos
import strategygames.abalone.format.Uci

import scala.util.Try

object Binary {
  // writeMove only used in tests for chess/draughts
  // def writeMove(m: String)             = Try(Writer.ply(gf, m))
  def writeMoves(ms: Iterable[String]): Try[Array[Byte]] = Try(Writer.plies(ms))

  def writeActionStrs(ms: ActionStrs): Try[Array[Byte]] = Try(Writer.actionStrs(ms))

  def readActionStrs(bs: List[Byte]): Try[ActionStrs] = Try(Reader actionStrs bs)

  def readActionStrs(bs: List[Byte], nb: Int): Try[ActionStrs] = Try(Reader.actionStrs(bs, nb))

  private object Reader {
    private val maxPlies = 600

    def actionStrs(bs: List[Byte]): ActionStrs = actionStrs(bs, maxPlies)

    def actionStrs(bs: List[Byte], nb: Int): ActionStrs = toActionStrs(intPlies(bs map toInt, nb))

    def toActionStrs(plies: List[String]): ActionStrs = plies.map(List(_))

    def intPlies(bs: List[Int], pliesToGo: Int): List[String] =
      bs match {
        case _ if pliesToGo <= 0 => Nil
        case Nil                 => Nil
        case b1 :: b2 :: rest    => moveUci(b1, b2) :: intPlies(rest, pliesToGo - 1)
        case x                   => !!(x map showByte mkString ",")
      }

    def moveUci(b1: Int, b2: Int): String = s"${posFromInt(b1)}${posFromInt(b2)}"

    // The maximal value of the index can go beyond 120 because of the strange order chosen in Piotr, so... 8 bits necessary for orig & dest
    def posFromInt(b: Int): String = Pos.fromIndex(right(b, 8)).toString()

    private def right(i: Int, x: Int): Int = i & lengthMasks(x)

    private val lengthMasks =
      Map(1 -> 0x01, 2 -> 0x03, 3 -> 0x07, 4 -> 0x0f, 5 -> 0x1f, 6 -> 0x3f, 7 -> 0x7f, 8 -> 0xff)

    private def !!(msg: String) = throw new Exception("Binary reader failed: " + msg)
  }

  private object Writer {
    def ply(str: String): List[Byte] = (str match {
      case Uci.Move.moveR(orig0, orig1, dest0, dest1) => moveUci(orig0 + orig1, dest0 + dest1)
      case _                                          => sys.error(s"Invalid move to write: $str")
    }).map(_.toByte)

    def plies(strs: Iterable[String]): Array[Byte] =
      strs.toList.flatMap(ply).to(Array)

    // can flatten because this GameLogic doesn't support multimove
    def actionStrs(strs: ActionStrs): Array[Byte] = plies(strs.flatten)

    def moveUci(src: String, dst: String): List[Int] = List(
      Pos.fromKey(src).get.index,
      Pos.fromKey(dst).get.index
    )
  }

  @inline private def toInt(b: Byte): Int = b & 0xff

  private def showByte(b: Int): String = "%08d" format b.toBinaryString.toInt
}
