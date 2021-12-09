package strategygames.fairysf
package format.pgn

import strategygames.GameFamily
import strategygames.fairysf.format.Uci

import scala.util.Try

object Binary {

  //writeMove only used in tests for chess/draughts
  //would need to reconsider how we do this when we write gameFamily at the start of the game
  //def writeMove(gf: GameFamily, m: String)             = Try(Writer.move(gf, m))
  def writeMoves(gf: GameFamily, ms: Iterable[String]) = Try(Writer.moves(gf, ms))

  def readMoves(bs: List[Byte])          = Try(Reader moves bs)
  def readMoves(bs: List[Byte], nb: Int) = Try(Reader.moves(bs, nb))

  private object MoveType {
    val Move = 0
    val Drop = 1
  }

  private object Reader {

    private val maxPlies = 600

    def moves(bs: List[Byte]): List[String]          = moves(bs, maxPlies)
    def moves(bs: List[Byte], nb: Int): List[String] = intMoves(bs map toInt, nb, None)

    def intMoves(bs: List[Int], pliesToGo: Int, gf: Option[GameFamily]): List[String] =
      (bs, gf) match {
        case (_, _) if pliesToGo <= 0 => Nil
        case (Nil, _)                 => Nil
        case (b1 :: rest, None)       => intMoves(rest, pliesToGo, Some(GameFamily(b1)))
        case (b1 :: b2 :: rest, Some(gf)) if headerBit(b1) == MoveType.Move =>
          moveUci(b1, b2) :: intMoves(rest, pliesToGo - 1, Some(gf))
        case (b1 :: b2 :: rest, Some(gf)) if headerBit(b1) == MoveType.Drop =>
          dropUci(gf, b1, b2) :: intMoves(rest, pliesToGo - 1, Some(gf))
        case (x, _) => !!(x map showByte mkString ",")
      }

    // 1 movetype (move or drop)
    // 7 pos (from)
    // ----
    // 1 promotion (bool)
    // 7 pos (dest)
    def moveUci(b1: Int, b2: Int): String =
      s"${posFromInt(b1)}${posFromInt(b2)}${promotionFromInt(b2)}"

    // 1 movetype
    // 7 pos (dest)
    // ----
    // 8 piece (only needs 4 bits?)
    def dropUci(gf: GameFamily, b1: Int, b2: Int): String =
      s"${pieceFromInt(gf, b2)}@${posFromInt(b1)}"

    def posFromInt(b: Int): String = Pos(right(b, 7)).get.toString()

    def promotionFromInt(b: Int): String = headerBit(b) match {
      case 1 => "+"
      case _ => ""
    }

    def pieceFromInt(gf: GameFamily, b: Int): String =
      Role.allByBinaryInt(gf).get(right(b, 7)).get.forsyth.toString

    private def headerBit(i: Int) = i >> 7
    //private def posString(i: Int) = fileChar(i >> 3).toString + rankChar(right(i, 3))
    //private def fileChar(i: Int)  = (i + 97).toChar
    //private def rankChar(i: Int)  = (i + 49).toChar

    private def right(i: Int, x: Int): Int           = i & lengthMasks(x)
    //private def cut(i: Int, from: Int, to: Int): Int = right(i, from) >> to
    //private def bitAt(i: Int, p: Int): Boolean       = cut(i, p, p - 1) != 0
    private val lengthMasks =
      Map(1 -> 0x01, 2 -> 0x03, 3 -> 0x07, 4 -> 0x0f, 5 -> 0x1f, 6 -> 0x3f, 7 -> 0x7f, 8 -> 0xff)
    private def !!(msg: String) = throw new Exception("Binary reader failed: " + msg)
  }

  private object Writer {

    def move(gf: GameFamily, str: String): List[Byte] =
      (str match {
        case Uci.Move.moveR(src, dst, promotion) => moveUci(src, dst, promotion)
        case Uci.Drop.dropR(piece, dst) => dropUci(gf, piece, dst)
        case _ => sys.error(s"Invalid move to write: ${str}")
      }) map (_.toByte)

    def moves(gf: GameFamily, strs: Iterable[String]): Array[Byte] =
      (gf.id.toByte :: strs.toList.flatMap(move(gf, _))).to(Array)

    def moveUci(src: String, dst: String, promotion: String) = List(
      (headerBit(MoveType.Move)) + Pos.fromKey(src).get.index,
      (headerBit(promotion.headOption match {
        case Some(_) => 1
        case None => 0
      })) + Pos.fromKey(dst).get.index
    )

    def dropUci(gf: GameFamily, piece: String, dst: String) = List(
      (headerBit(MoveType.Drop)) + Pos.fromKey(dst).get.index,
      Role.allByForsyth(gf).get(piece(0)).get.binaryInt
    )

    private def headerBit(i: Int) = i << 7

  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String    = "%08d" format (b.toBinaryString.toInt)
}
