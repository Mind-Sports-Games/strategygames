package strategygames.abalone
package format.pgn

import strategygames.ActionStrs
import strategygames.abalone.format.Uci

import scala.util.Try

// TODO Abalone. Need to rewrite most of this to make binary storage of games
// efficient and correct for Abalone. Speak to Matt as he has done this for most games

object Binary {

  // writeMove only used in tests for chess/draughts
  // def writeMove(m: String)             = Try(Writer.ply(gf, m))
  def writeMoves(ms: Iterable[String]) = Try(Writer.plies(ms))

  def writeActionStrs(ms: ActionStrs) = Try(Writer.actionStrs(ms))

  def readActionStrs(bs: List[Byte])          = Try(Reader actionStrs bs)
  def readActionStrs(bs: List[Byte], nb: Int) = Try(Reader.actionStrs(bs, nb))

  // No MoveType implemented for Delimiter/Multimove
  // MoveType could be removed as there are no Drops in Mancala
  // similarly gamefamily doesnt need to be stored either but we do
  private object MoveType {
    val Move = 0
    val Drop = 1
  }

  private object Reader {

    private val maxPlies = 600

    def actionStrs(bs: List[Byte]): ActionStrs          = actionStrs(bs, maxPlies)
    def actionStrs(bs: List[Byte], nb: Int): ActionStrs = toActionStrs(intPlies(bs map toInt, nb))

    def toActionStrs(plies: List[String]): ActionStrs = plies.map(List(_))

    def intPlies(bs: List[Int], pliesToGo: Int): List[String] =
      bs match {
        case _ if pliesToGo <= 0                                  => Nil
        case Nil                                                  => Nil
        case (b1 :: b2 :: rest) if headerBit(b1) == MoveType.Move =>
          moveUci(b1, b2) :: intPlies(rest, pliesToGo - 1)
        case x                                                    => !!(x map showByte mkString ",")
      }

    // TODO: This is hugely inefficient for Abalone - speak to Matt about how to improve this
    // 1 movetype (move or drop)
    // 7 pos (from)
    // ----
    // 1 promotion (bool)
    // 7 pos (dest)
    def moveUci(b1: Int, b2: Int): String =
      s"${posFromInt(b1)}${posFromInt(b2)}${promotionFromInt(b2)}"

    def posFromInt(b: Int): String = Pos(right(b, 7)).get.toString()

    // not even possible in Abalone
    def promotionFromInt(b: Int): String = headerBit(b) match {
      case 1 => "+"
      case _ => ""
    }

    private def headerBit(i: Int) = i >> 7

    private def right(i: Int, x: Int): Int = i & lengthMasks(x)
    private val lengthMasks                =
      Map(1 -> 0x01, 2 -> 0x03, 3 -> 0x07, 4 -> 0x0f, 5 -> 0x1f, 6 -> 0x3f, 7 -> 0x7f, 8 -> 0xff)
    private def !!(msg: String)            = throw new Exception("Binary reader failed: " + msg)
  }

  private object Writer {

    def ply(str: String): List[Byte] =
      (str match {
        case Uci.Move.moveR(src, dst, promotion) => moveUci(src, dst, promotion)
        case _                                   => sys.error(s"Invalid move to write: ${str}")
      }) map (_.toByte)

    def plies(strs: Iterable[String]): Array[Byte] =
      strs.toList.flatMap(ply).to(Array)

    // can flatten because this GameLogic doesn't support multimove
    def actionStrs(strs: ActionStrs): Array[Byte] = plies(strs.flatten)

    def moveUci(src: String, dst: String, promotion: String) = List(
      (headerBit(MoveType.Move)) + Pos.fromKey(src).get.index,
      (headerBit(promotion.headOption match {
        case Some(_) => 1
        case None    => 0
      })) + Pos.fromKey(dst).get.index
    )

    private def headerBit(i: Int) = i << 7

  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String    = "%08d" format (b.toBinaryString.toInt)
}
