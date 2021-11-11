package strategygames.fairysf
package format.pgn

import strategygames.GameFamily
import strategygames.fairysf.format.Uci

import scala.util.Try

object Binary {

  def writeMove(gf: GameFamily, m: String)             = Try(Writer.move(gf, m))
  def writeMoves(gf: GameFamily, ms: Iterable[String]) = Try(Writer.moves(gf, ms))

  def readMoves(bs: List[Byte])          = Try(Reader moves bs)
  def readMoves(bs: List[Byte], nb: Int) = Try(Reader.moves(bs, nb))

  private object MoveType {
    val Move = 0
    val Drop = 1
  }

  //private object Encoding {
  //  val pieceInts: Map[String, Int] =
  //    Map("K" -> 1, "Q" -> 2, "R" -> 3, "N" -> 4, "B" -> 5, "O-O" -> 6, "O-O-O" -> 7, "L" -> 8)
  //  val pieceStrs: Map[Int, String]     = pieceInts map { case (k, v) => v -> k }
  //  val dropPieceInts: Map[String, Int] = Map("P" -> 1, "Q" -> 2, "R" -> 3, "N" -> 4, "B" -> 5)
  //  val dropPieceStrs: Map[Int, String] = dropPieceInts map { case (k, v) => v -> k }
  //  val promotionInts: Map[String, Int] = Map("" -> 0, "Q" -> 1, "R" -> 2, "N" -> 3, "B" -> 4, "K" -> 6)
  //  val promotionStrs: Map[Int, String] = promotionInts map { case (k, v) => v -> k }
  //  val checkInts: Map[String, Int]     = Map("" -> 0, "+" -> 1, "#" -> 2)
  //  val checkStrs: Map[Int, String]     = checkInts map { case (k, v) => v -> k }
  //}

  private object Reader {

    private val maxPlies = 600

    def moves(bs: List[Byte]): List[String]          = moves(bs, maxPlies)
    def moves(bs: List[Byte], nb: Int): List[String] = intMoves(bs map toInt, nb)

    def intMoves(bs: List[Int], pliesToGo: Int): List[String] =
      bs match {
        case _ if pliesToGo <= 0 => Nil
        case Nil                 => Nil
        case b1 :: b2 :: rest if moveType(b1) == MoveType.Move =>
          moveUci(b1, b2) :: intMoves(rest, pliesToGo - 1)
        case b1 :: b2 :: rest if moveType(b1) == MoveType.Drop =>
          dropUci(b1, b2) :: intMoves(rest, pliesToGo - 1)
        case x => !!(x map showByte mkString ",")
      }

    // 1 movetype
    // 7 pos (from)
    // ----
    // 1 gamefamily
    // 7 pos (dest)
    def moveUci(b1: Int, b2: Int): String =
      s"${posFromInt(b1)}${posFromInt(b2)}"

    def dropUci(b1: Int, b2: Int): String =
      s"${pieceFromInt(gameFamily(b2), b1)}@${posFromInt(b2)}"

    def posFromInt(b: Int): String = Pos(right(b, 7)).get.toString()

    def pieceFromInt(gf: Int, b: Int): String =
      Role.allByBinaryInt(GameFamily(gf)).get(b).get.forsyth.toString

    private def moveType(i: Int)  = i >> 7
    private def gameFamily(i: Int) = (i >> 7) + 3
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
        case Uci.Move.moveR(src, dst, _) => moveUci(src, dst)
        case Uci.Drop.dropR(piece, dst, _) => dropUci(gf, piece, dst)
        case _ => sys.error(s"Invalid move to write: ${str}")
      }) map (_.toByte)

    def moves(gf: GameFamily, strs: Iterable[String]): Array[Byte] =
      strs.flatMap(move(gf, _)).to(Array)

    def moveUci(src: String, dst: String) = List(
      (MoveType.Move << 7) + Pos.fromKey(src).get.index,
      (0 << 7) + Pos.fromKey(dst).get.index//0 << 7 is gameFamily 0 can be Shogi for now
    )

    def dropUci(gf: GameFamily, piece: String, dst: String) = List(
      (MoveType.Drop << 7) + Role.allByForsyth.get(piece(0)).get.binaryInt,
      (gf.id - 3 << 7) + Pos.fromKey(dst).get.index//-3 to set to 0 or 1 (Shogi or Xiangqi)
    )

    //val pieceR       = "([KQRNBL])"
    //val posR         = "([a-i][1-9]|[a-i]10)"
    //val MoveR        = s"^$posR$posR$$".r
    //val DropR        = s"^([QRNBP])@$posR$checkR$$".r
  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String    = "%08d" format (b.toBinaryString.toInt)
}
