package strategygames.fiveinarow
package format.pgn

import strategygames.fiveinarow.format.Uci
import strategygames.ActionStrs

import scala.util.Try

object Binary {

  def writeMove(m: String)             = Try(Writer.ply(m, continuesTurn = false))
  def writeMoves(ms: Iterable[String]) = Try(Writer.plies(ms.map(List(_))))

  def writeActionStrs(ms: ActionStrs) = Try(Writer.plies(ms.map(_.toList)))

  def readActionStrs(bs: List[Byte])          = Try(Reader actionStrs bs)
  def readActionStrs(bs: List[Byte], nb: Int) = Try(Reader.actionStrs(bs, nb))

  private object ActionType {
    val Other = 0
    val Drop  = 1
    val Swap  = 2
    val Swap2 = 3
  }

  private object OtherType {
    val Offer10 = 1
  }

  private val continuesBit = 1 << 5

  private def right(i: Int, x: Int): Int = i & lengthMasks(x)
  private val lengthMasks                =
    Map(1 -> 0x01, 2 -> 0x03, 3 -> 0x07, 4 -> 0x0f, 5 -> 0x1f, 6 -> 0x3f, 7 -> 0x7f, 8 -> 0xff)

  private object Reader {

    private val maxPlies = 1000

    def actionStrs(bs: List[Byte]): ActionStrs          = actionStrs(bs, maxPlies)
    def actionStrs(bs: List[Byte], nb: Int): ActionStrs = toActionStrs(intPlies(bs map toInt, nb))

    def toActionStrs(plies: List[(String, Boolean)]): ActionStrs =
      plies
        .foldLeft(Vector.empty[Vector[String]]) {
          case (turns, (action, true)) if turns.nonEmpty => turns.init :+ (turns.last :+ action)
          case (turns, (action, _))                      => turns :+ Vector(action)
        }
        .map(_.toList)
        .toList

    def intPlies(bs: List[Int], pliesToGo: Int): List[(String, Boolean)] =
      bs match {
        case _ if pliesToGo <= 0 => Nil
        case Nil                 => Nil
        case b1 :: rest          =>
          val continues = (b1 & continuesBit) != 0
          (headerBit(b1), rest) match {
            case (ActionType.Drop, b2 :: more)                                 =>
              (dropUci(b1, b2), continues) :: intPlies(more, pliesToGo - 1)
            case (ActionType.Swap, more)                                       =>
              ("swap", continues) :: intPlies(more, pliesToGo - 1)
            case (ActionType.Swap2, more)                                      =>
              ("swap2", continues) :: intPlies(more, pliesToGo - 1)
            case (ActionType.Other, more) if right(b1, 2) == OtherType.Offer10 =>
              ("offer10", continues) :: intPlies(more, pliesToGo - 1)
            case _                                                             =>
              !!(bs map showByte mkString ",")
          }
      }

    private def pos(b1: Int, b2: Int): Pos = Pos((right(b1 >> 2, 1) << 8) + b2).get

    def dropUci(b1: Int, b2: Int): String =
      s"${Role.binaryInt(right(b1, 2)).get.pgn}@${pos(b1, b2).key}"

    private def headerBit(i: Int) = i >> 6

    private def !!(msg: String) = throw new Exception("Binary reader failed: " + msg)
  }

  private object Writer {

    def ply(str: String, continuesTurn: Boolean): List[Byte] = {
      val continues = if (continuesTurn) continuesBit else 0
      (str match {
        case Uci.Drop.dropR(role, dst) =>
          pointed(headerBit(ActionType.Drop) + Role.allByPgn(role.head).binaryInt, dst)
        case Uci.Swap.swapR()          => List(headerBit(ActionType.Swap))
        case Uci.Swap2.swap2R()        => List(headerBit(ActionType.Swap2))
        case Uci.Offer10.offer10R()    => List(headerBit(ActionType.Other) + OtherType.Offer10)
        case _                         => sys.error(s"Invalid action to write: ${str}")
      }) match {
        case first :: rest => ((first + continues) :: rest).map(_.toByte)
        case Nil           => Nil
      }
    }

    def plies(turns: Iterable[List[String]]): Array[Byte] =
      turns.toList.flatMap(_.zipWithIndex.flatMap { case (a, i) => ply(a, continuesTurn = i > 0) }).to(Array)

    private def pointed(first: Int, dst: String): List[Int] = {
      val index = Pos.fromKey(dst).get.index
      List(first + ((index >> 8) << 2), right(index, 8))
    }

    private def headerBit(i: Int) = i << 6

  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String    = "%08d" format (b.toBinaryString.toInt)
}
