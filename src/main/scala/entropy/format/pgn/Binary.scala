package strategygames.entropy
package format.pgn

import strategygames.entropy.format.Uci
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
    val Move        = 0
    val Drop        = 1
    val Pass        = 2
    val DrawCounter = 3
  }

  private def right(i: Int, x: Int): Int = i & lengthMasks(x)
  private val lengthMasks                =
    Map(1 -> 0x01, 2 -> 0x03, 3 -> 0x07, 4 -> 0x0f, 5 -> 0x1f, 6 -> 0x3f, 7 -> 0x7f, 8 -> 0xff)

  private object Reader {

    private val maxPlies = 1000

    def actionStrs(bs: List[Byte]): ActionStrs          = actionStrs(bs, maxPlies)
    def actionStrs(bs: List[Byte], nb: Int): ActionStrs = toActionStrs(intPlies(bs map toInt, nb))

    // Entropy needs no delimiter: a draw is always followed by the drop that spends it,
    // and a move or a pass is a turn on its own, so the grouping falls out of the actions.
    def toActionStrs(plies: List[String]): ActionStrs =
      plies
        .foldLeft(Vector.empty[Vector[String]]) { (turns, ply) =>
          if (turns.nonEmpty && turns.last.headOption.exists(isDraw) && turns.last.size == 1)
            turns.updated(turns.size - 1, turns.last :+ ply)
          else turns :+ Vector(ply)
        }

    private def isDraw(s: String) = s.startsWith("draw")

    def intPlies(bs: List[Int], pliesToGo: Int): List[String] =
      bs match {
        case _ if pliesToGo <= 0                                     => Nil
        case Nil                                                     => Nil
        case (b1 :: b2 :: rest) if headerBit(b1) == ActionType.Move  =>
          moveUci(b1, b2) :: intPlies(rest, pliesToGo - 1)
        case (b1 :: b2 :: rest) if headerBit(b1) == ActionType.Drop  =>
          dropUci(b1, b2) :: intPlies(rest, pliesToGo - 1)
        case (b1 :: rest) if headerBit(b1) == ActionType.Pass        =>
          passUci :: intPlies(rest, pliesToGo - 1)
        case (b1 :: rest) if headerBit(b1) == ActionType.DrawCounter =>
          drawUci(b1) :: intPlies(rest, pliesToGo - 1)
        case x                                                       => !!(x map showByte mkString ",")
      }

    def moveUci(b1: Int, b2: Int): String = s"${posFromInt(b1)}${posFromInt(b2)}"

    def dropUci(b1: Int, b2: Int): String = s"${roleFromInt(b1)}@${posFromInt(b2)}"

    val passUci = "pass"

    def drawUci(b1: Int): String = s"draw-${roleFromInt(b1)}"

    def posFromInt(b: Int): String = Pos(right(b, 6)).get.key
    def roleFromInt(b: Int): Char  = Role.binaryInt(right(b, 6)).get.forsyth

    private def headerBit(i: Int) = i >> 6

    private def !!(msg: String) = throw new Exception("Binary reader failed: " + msg)
  }

  private object Writer {

    def ply(str: String): List[Byte] =
      (str match {
        case Uci.Move.moveR(orig, dest)         => moveUci(orig, dest)
        case Uci.Drop.dropR(role, dst)          => dropUci(role, dst)
        case Uci.Pass.passR()                   => passUci
        case Uci.DrawCounter.drawCounterR(role) => drawUci(role)
        case _                                  => sys.error(s"Invalid action to write: ${str}")
      }) map (_.toByte)

    def plies(strs: Iterable[String]): Array[Byte] =
      strs.toList.flatMap(ply).to(Array)

    // safe to flatten: the reader regroups turns from the actions themselves
    def actionStrs(strs: ActionStrs): Array[Byte] = plies(strs.flatten)

    def moveUci(orig: String, dest: String) = List(
      headerBit(ActionType.Move) + Pos.fromKey(orig).get.index,
      Pos.fromKey(dest).get.index
    )

    def dropUci(role: String, dst: String) = List(
      headerBit(ActionType.Drop) + roleInt(role),
      Pos.fromKey(dst).get.index
    )

    val passUci = List(headerBit(ActionType.Pass))

    def drawUci(role: String) = List(headerBit(ActionType.DrawCounter) + roleInt(role))

    private def roleInt(role: String): Int =
      Role.allByForsyth(role.head).binaryInt

    private def headerBit(i: Int) = i << 6

  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String    = "%08d" format (b.toBinaryString.toInt)
}
