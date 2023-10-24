package strategygames.go
package format.pgn

import strategygames.go.format.Uci
import strategygames.Actions

import scala.util.Try

object Binary {

  // writeMove only used in tests
  def writeMove(m: String)             = Try(Writer.ply(m))
  def writeMoves(ms: Iterable[String]) = Try(Writer.plies(ms))

  def writeActions(ms: Actions) = Try(Writer.actions(ms))

  def readActions(bs: List[Byte])          = Try(Reader actions bs)
  def readActions(bs: List[Byte], nb: Int) = Try(Reader.actions(bs, nb))

  // No MoveType implemented for Delimiter/Multimove

  private object MoveType {
    val Pass          = 0
    val Drop          = 1
    val SelectSquares = 2
  }

  private def right(i: Int, x: Int): Int = i & lengthMasks(x)
  private val lengthMasks                =
    Map(1 -> 0x01, 2 -> 0x03, 3 -> 0x07, 4 -> 0x0f, 5 -> 0x1f, 6 -> 0x3f, 7 -> 0x7f, 8 -> 0xff)

  private object Reader {

    private val maxPlies = 600

    def actions(bs: List[Byte]): Actions          = actions(bs, maxPlies)
    def actions(bs: List[Byte], nb: Int): Actions = toActions(intPlies(bs map toInt, nb))

    def toActions(plies: List[String]): Actions = plies.map(List(_))

    def intPlies(bs: List[Int], pliesToGo: Int): List[String] =
      bs match {
        case _ if pliesToGo <= 0                                           => Nil
        case Nil                                                           => Nil
        case (b1 :: b2 :: rest) if headerBit(b1) == MoveType.Drop          =>
          dropUci(b1, b2) :: intPlies(rest, pliesToGo - 1)
        case (b1 :: rest) if headerBit(b1) == MoveType.Pass                =>
          passUci :: intPlies(rest, pliesToGo - 1)
        case (b1 :: b2 :: rest) if headerBit(b1) == MoveType.SelectSquares => {
          val numDS = numDeadStones(b1, b2)
          val ds    = deadStones(rest, numDS)
          selectSquaresUci(ds) :: intPlies(rest.drop(numDS * 2), pliesToGo - 1)
        }
        case x                                                             => !!(x map showByte mkString ",")
      }

    def deadStones(bs: List[Int], stonesToGo: Int): List[String] =
      bs match {
        case _ if stonesToGo <= 0                                          => Nil
        case (b1 :: b2 :: rest) if headerBit(b1) == MoveType.SelectSquares =>
          posFromInt(b1, b2) :: deadStones(rest, stonesToGo - 1)
        case x                                                             => !!(x map showByte mkString ",")
      }

    // 2 movetype
    // 6 pos (dest) first 6 bits (but if only needing up to 361 this will essentially leave 5 bits free in the middle and only use the least significant bit)
    // ----
    // 8 pos (dest) last 8 bits. Total 2**14 = 16384
    def dropUci(b1: Int, b2: Int): String =
      s"s@${posFromInt(b1, b2)}"

    val passUci                                    = "pass"
    def selectSquaresUci(ds: List[String]): String = s"ss:${ds.mkString(",")}"
    def numDeadStones(b1: Int, b2: Int): Int       = (right(b1, 6) << 8) + b2
    def posFromInt(b1: Int, b2: Int): String       = Pos((right(b1, 6) << 8) + b2).get.toString()

    private def headerBit(i: Int) = i >> 6

    private def !!(msg: String) = throw new Exception("Binary reader failed: " + msg)
  }

  private object Writer {

    def ply(str: String): List[Byte] =
      (str match {
        case Uci.Drop.dropR(_, dst)               => dropUci(dst)
        case Uci.Pass.passR()                     => passUci
        case Uci.SelectSquares.selectSquaresR(ss) => selectSquaresUci(ss)
        case _                                    => sys.error(s"Invalid move to write: ${str}")
      }) map (_.toByte)

    def plies(strs: Iterable[String]): Array[Byte] =
      strs.toList.flatMap(ply).to(Array)

    // can flatten because this GameLogic doesn't support multimove
    def actions(strs: Actions): Array[Byte] = plies(strs.flatten)

    def passUci = List(headerBit(MoveType.Pass))

    def selectSquaresUci(selectedSquares: String) = {
      val squares: List[String] =
        if (selectedSquares == "") List[String]().empty else selectedSquares.split(",").toList
      val numDeadStones         = squares.length
      val start                 = List(
        (headerBit(MoveType.SelectSquares)) + (numDeadStones >> 8),
        right(numDeadStones, 8)
      )
      val stones                = squares.map(dst =>
        List(
          headerBit(MoveType.SelectSquares) + (Pos.fromKey(dst).get.index >> 8),
          right(Pos.fromKey(dst).get.index, 8)
        )
      )
      start ::: stones.flatten
    }

    def dropUci(dst: String) = List(
      (headerBit(MoveType.Drop)) + (Pos.fromKey(dst).get.index >> 8),
      right(Pos.fromKey(dst).get.index, 8)
    )

    private def headerBit(i: Int) = i << 6

  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String    = "%08d" format (b.toBinaryString.toInt)
}
