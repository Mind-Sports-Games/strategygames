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

  // A square index on the 12x12 board runs from 0 to 143 and needs the whole byte, so a
  // move is written as two bytes, the origin and the destination, and a pass as the single
  // byte 0xff, which no square index reaches.
  private val passByte = 0xff

  private object Reader {

    private val maxPlies = 1000

    def actionStrs(bs: List[Byte]): ActionStrs          = actionStrs(bs, maxPlies)
    def actionStrs(bs: List[Byte], nb: Int): ActionStrs = toActionStrs(intPlies(bs map toInt, nb))

    // every action is a turn of its own
    def toActionStrs(plies: List[String]): ActionStrs = plies.map(Vector(_)).toVector

    def intPlies(bs: List[Int], pliesToGo: Int): List[String] =
      bs match {
        case _ if pliesToGo <= 0                              => Nil
        case Nil                                              => Nil
        case b1 :: rest if b1 == passByte                     => passUci :: intPlies(rest, pliesToGo - 1)
        case b1 :: b2 :: rest if isSquare(b1) && isSquare(b2) =>
          moveUci(b1, b2) :: intPlies(rest, pliesToGo - 1)
        case x                                                => !!(x map showByte mkString ",")
      }

    def moveUci(b1: Int, b2: Int): String = s"${posFromInt(b1)}${posFromInt(b2)}"

    val passUci = "pass"

    def isSquare(b: Int): Boolean  = b < Pos.allSize
    def posFromInt(b: Int): String = Pos(b).get.key

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
      Pos.fromKey(orig).get.index,
      Pos.fromKey(dest).get.index
    )

    val passUci = List(passByte)

  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String    = "%08d" format (b.toBinaryString.toInt)
}
