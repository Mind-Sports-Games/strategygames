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

  // Every action is two bytes. A move is its origin square index then its destination square
  // index, each 0 to 143 on the 12x12 board. A pass is two bytes of 0xff, which is no square.
  private val passByte = 0xff

  private object Reader {

    // If changing this, consider changing other gamelogics and also lila game maxPlies.
    // Yonkoma never gets near it: the longest legal game is 83 plies on the 10x10 board and
    // 121 on the 12x12 board, passes counted (a longest-path search over every position)
    private val maxPlies = 1000

    def actionStrs(bs: List[Byte]): ActionStrs          = actionStrs(bs, maxPlies)
    def actionStrs(bs: List[Byte], nb: Int): ActionStrs = toActionStrs(intPlies(bs map toInt, nb))

    // every action is a turn of its own
    def toActionStrs(plies: List[String]): ActionStrs = plies.map(Vector(_)).toVector

    def intPlies(bs: List[Int], pliesToGo: Int): List[String] =
      bs match {
        case _ if pliesToGo <= 0              => Nil
        case Nil                              => Nil
        case `passByte` :: `passByte` :: rest => passUci :: intPlies(rest, pliesToGo - 1)
        case orig :: dest :: rest             => moveUci(orig, dest) :: intPlies(rest, pliesToGo - 1)
        case x                                => !!(x map showByte mkString ",")
      }

    def moveUci(orig: Int, dest: Int): String = s"${posFromInt(orig)}${posFromInt(dest)}"

    val passUci = "pass"

    def posFromInt(b: Int): String = Pos(b).getOrElse(!!(s"no square ${b}")).key

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

    def moveUci(orig: String, dest: String) = List(Pos.fromKey(orig).get.index, Pos.fromKey(dest).get.index)

    def passUci = List(passByte, passByte)

  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String    = "%08d" format (b.toBinaryString.toInt)
}
