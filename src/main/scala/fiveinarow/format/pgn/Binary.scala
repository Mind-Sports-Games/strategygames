package strategygames.fiveinarow
package format.pgn

import strategygames.fiveinarow.format.Uci
import strategygames.ActionStrs

import scala.util.Try

object Binary {

  // writeMove only used in tests
  def writeMove(m: String)             = Try(Writer.ply(m))
  def writeMoves(ms: Iterable[String]) = Try(Writer.plies(ms))

  def writeActionStrs(ms: ActionStrs) = Try(Writer.actionStrs(ms))

  def readActionStrs(bs: List[Byte])          = Try(Reader actionStrs bs)
  def readActionStrs(bs: List[Byte], nb: Int) = Try(Reader.actionStrs(bs, nb))

  private object ActionType {
    val Drop  = 1
    val Swap  = 2
    val Swap2 = 3
  }

  private def right(i: Int, x: Int): Int = i & lengthMasks(x)
  private val lengthMasks                =
    Map(1 -> 0x01, 2 -> 0x03, 3 -> 0x07, 4 -> 0x0f, 5 -> 0x1f, 6 -> 0x3f, 7 -> 0x7f, 8 -> 0xff)

  private object Reader {

    private val maxPlies = 1000

    def actionStrs(bs: List[Byte]): ActionStrs          = actionStrs(bs, maxPlies)
    def actionStrs(bs: List[Byte], nb: Int): ActionStrs = toActionStrs(intPlies(bs map toInt, nb))

    // every game starts from an empty board, so the turns are the opening's three drops,
    // a swap2 with the two drops that follow it, and single actions otherwise
    def toActionStrs(plies: List[String]): ActionStrs = {
      val (opening, rest)                             = plies.splitAt(3)
      def turns(ps: List[String]): List[List[String]] = ps match {
        case Nil                   => Nil
        case (s @ "swap2") :: rest => (s :: rest.take(2)) :: turns(rest.drop(2))
        case action :: rest        => List(action) :: turns(rest)
      }
      (if (opening.isEmpty) Nil else List(opening)) ::: turns(rest)
    }

    def intPlies(bs: List[Int], pliesToGo: Int): List[String] =
      bs match {
        case _ if pliesToGo <= 0                                    => Nil
        case Nil                                                    => Nil
        case (b1 :: b2 :: rest) if headerBit(b1) == ActionType.Drop =>
          dropUci(b1, b2) :: intPlies(rest, pliesToGo - 1)
        case (b1 :: rest) if headerBit(b1) == ActionType.Swap       =>
          "swap" :: intPlies(rest, pliesToGo - 1)
        case (b1 :: rest) if headerBit(b1) == ActionType.Swap2      =>
          "swap2" :: intPlies(rest, pliesToGo - 1)
        case x                                                      => !!(x map showByte mkString ",")
      }

    def dropUci(b1: Int, b2: Int): String =
      s"${Role.binaryInt(right(b1, 6)).get.pgn}@${Pos(b2).get.key}"

    private def headerBit(i: Int) = i >> 6

    private def !!(msg: String) = throw new Exception("Binary reader failed: " + msg)
  }

  private object Writer {

    def ply(str: String): List[Byte] =
      (str match {
        case Uci.Drop.dropR(role, dst) => dropUci(role, dst)
        case Uci.Swap.swapR()          => List(headerBit(ActionType.Swap))
        case Uci.Swap2.swap2R()        => List(headerBit(ActionType.Swap2))
        case _                         => sys.error(s"Invalid action to write: ${str}")
      }) map (_.toByte)

    def plies(strs: Iterable[String]): Array[Byte] =
      strs.toList.flatMap(ply).to(Array)

    // safe to flatten: the reader regroups turns from the actions themselves
    def actionStrs(strs: ActionStrs): Array[Byte] = plies(strs.flatten)

    def dropUci(role: String, dst: String) = List(
      headerBit(ActionType.Drop) + Role.allByPgn(role.head).binaryInt,
      Pos.fromKey(dst).get.index
    )

    private def headerBit(i: Int) = i << 6

  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String    = "%08d" format (b.toBinaryString.toInt)
}
