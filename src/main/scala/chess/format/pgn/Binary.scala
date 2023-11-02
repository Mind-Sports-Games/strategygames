package strategygames.chess
package format.pgn

import scala.util.Try
import strategygames.ActionStrs

object Binary {

  def writeMove(m: String)             = Try(Writer ply m)
  def writeMoves(ms: Iterable[String]) = Try(Writer plies ms)

  def writeActionStrs(ms: ActionStrs) = Try(Writer actionStrs ms)

  def readActionStrs(bs: List[Byte])          = Try(Reader actionStrs bs)
  def readActionStrs(bs: List[Byte], nb: Int) = Try(Reader.actionStrs(bs, nb))

  private object MoveType {
    val SimplePawn  = 0
    val SimplePiece = 1
    val FullPawn    = 2
    val FullPiece   = 3
  }

  private object Delimiter {
    val str = ""
    val int = 0
  }

  private object Encoding {
    val pieceInts: Map[String, Int]     =
      Map("K" -> 1, "Q" -> 2, "R" -> 3, "N" -> 4, "B" -> 5, "O-O" -> 6, "O-O-O" -> 7, "L" -> 8)
    val pieceStrs: Map[Int, String]     = pieceInts map { case (k, v) => v -> k }
    val dropPieceInts: Map[String, Int] = Map("P" -> 1, "Q" -> 2, "R" -> 3, "N" -> 4, "B" -> 5)
    val dropPieceStrs: Map[Int, String] = dropPieceInts map { case (k, v) => v -> k }
    val promotionInts: Map[String, Int] = Map("" -> 0, "Q" -> 1, "R" -> 2, "N" -> 3, "B" -> 4, "K" -> 6)
    val promotionStrs: Map[Int, String] = promotionInts map { case (k, v) => v -> k }
    val checkInts: Map[String, Int]     = Map("" -> 0, "+" -> 1, "#" -> 2)
    val checkStrs: Map[Int, String]     = checkInts map { case (k, v) => v -> k }
  }

  private object Reader {

    import Encoding._

    private val maxPlies = 600

    def actionStrs(bs: List[Byte]): ActionStrs          = actionStrs(bs, maxPlies)
    def actionStrs(bs: List[Byte], nb: Int): ActionStrs = toActionStrs(intPlies(bs map toInt, nb))

    def toActionStrs(plies: List[String]): ActionStrs =
      if (plies.contains(Delimiter.str)) unflatten(plies)
      else plies.map(List(_))

    def unflatten(plies: List[String]): List[List[String]] =
      if (plies.size == 0) List()
      else plies.takeWhile(_ != Delimiter.str) :: unflatten(plies.dropWhile(_ != Delimiter.str).drop(1))

    def intPlies(bs: List[Int], pliesToGo: Int): List[String] =
      bs match {
        case _ if pliesToGo <= 0                                          => Nil
        case Nil                                                          => Nil
        case b1 :: rest if b1 == Delimiter.int                            =>
          Delimiter.str :: intPlies(rest, pliesToGo)
        case b1 :: rest if moveType(b1) == MoveType.SimplePawn            =>
          simplePawn(b1) :: intPlies(rest, pliesToGo - 1)
        case b1 :: b2 :: rest if moveType(b1) == MoveType.SimplePiece     =>
          simplePiece(b1, b2) :: intPlies(rest, pliesToGo - 1)
        case b1 :: b2 :: rest if moveType(b1) == MoveType.FullPawn        =>
          fullPawn(b1, b2) :: intPlies(rest, pliesToGo - 1)
        case b1 :: b2 :: b3 :: rest if moveType(b1) == MoveType.FullPiece =>
          fullPiece(b1, b2, b3) :: intPlies(rest, pliesToGo - 1)
        case x                                                            => !!(x map showByte mkString ",")
      }

    // 0 => 00000000 => marker for end of turn.
    // This would normally map to simplePawn
    // but there is no way for a simple pawn move to end on a1 (pos 0)

    def simplePawn(i: Int): String = posString(right(i, 6))

    // 2 movetype
    // 6 pos
    // ----
    // 4 role (but not fully used)
    // 2 check
    // 1 capture
    // 1 drop
    def simplePiece(b1: Int, b2: Int): String =
      if (bitAt(b2, 1)) drop(b1, b2)
      else
        pieceStrs(b2 >> 4) match {
          case castle @ ("O-O" | "O-O-O") =>
            val check = checkStrs(cut(b2, 4, 2))
            s"$castle$check"
          case piece                      =>
            val pos     = posString(right(b1, 6))
            val capture = if (bitAt(b2, 2)) "x" else ""
            val check   = checkStrs(cut(b2, 4, 2))
            s"$piece$capture$pos$check"
        }

    def drop(b1: Int, b2: Int): String = {
      val piece = dropPieceStrs(b2 >> 4)
      val pos   = posString(right(b1, 6))
      val check = checkStrs(cut(b2, 4, 2))
      s"$piece@$pos$check"
    }

    def fullPawn(b1: Int, b2: Int): String = {
      val pos         = posString(right(b1, 6))
      val fileCapture = (b2 >> 6) match {
        case 1 => s"${(pos(0) - 1).toChar}x"
        case 2 => s"${(pos(0) + 1).toChar}x"
        case _ => ""
      }
      val check       = checkStrs(cut(b2, 6, 4))
      val prom        = promotionStrs(cut(b2, 4, 1))
      val promotion   = if (prom.isEmpty) "" else s"=$prom"
      s"$fileCapture$pos$promotion$check"
    }

    def fullPiece(b1: Int, b2: Int, b3: Int): String = {
      val pos     = posString(right(b1, 6))
      val piece   = pieceStrs(b2 >> 4)
      val capture = if (bitAt(b2, 2)) "x" else ""
      val check   = checkStrs(cut(b2, 4, 2))
      val disamb  = (b3 >> 6) match {
        case 0 => fileChar(right(b3, 3)).toString
        case 1 => rankChar(right(b3, 3)).toString
        case _ => posString(right(b3, 6))
      }
      s"$piece$disamb$capture$pos$check"
    }

    private def moveType(i: Int)  = i >> 6
    private def posString(i: Int) = fileChar(i >> 3).toString + rankChar(right(i, 3))
    private def fileChar(i: Int)  = (i + 97).toChar
    private def rankChar(i: Int)  = (i + 49).toChar

    private def right(i: Int, x: Int): Int           = i & lengthMasks(x)
    private def cut(i: Int, from: Int, to: Int): Int = right(i, from) >> to
    private def bitAt(i: Int, p: Int): Boolean       = cut(i, p, p - 1) != 0
    private val lengthMasks                          =
      Map(1 -> 0x01, 2 -> 0x03, 3 -> 0x07, 4 -> 0x0f, 5 -> 0x1f, 6 -> 0x3f, 7 -> 0x7f, 8 -> 0xff)
    private def !!(msg: String)                      = throw new Exception("Binary reader failed: " + msg)
  }

  private object Writer {

    import Encoding._

    def ply(str: String): List[Byte] =
      (str match {
        case Delimiter.str                                => List(Delimiter.int)
        case pos if pos.length == 2                       => simplePawn(pos)
        case CastlingR(str, check)                        => castling(str, check)
        case SimplePieceR(piece, capture, pos, check)     =>
          simplePiece(piece, pos, capture, check)
        case FullPawnR(file, pos, promotion, check)       =>
          fullPawn(Option(file), pos, check, Option(promotion))
        case FullPieceR(piece, orig, capture, pos, check) =>
          fullPiece(piece, orig, pos, capture, check)
        case DropR(role, pos, check)                      => drop(role, pos, check)
      }) map (_.toByte)

    def plies(strs: Iterable[String]): Array[Byte] = strs.flatMap(ply).to(Array)

    def actionStrs(strs: ActionStrs): Array[Byte] =
      if (strs.size == 0 || strs.map(_.size).max == 1) plies(strs.flatten)
      else plies(strs.toList.map(_.toList :+ "").flatten)

    def simplePawn(pos: String) =
      List(
        (MoveType.SimplePawn << 6) + posInt(pos)
      )

    def simplePiece(piece: String, pos: String, capture: String, check: String) =
      List(
        (MoveType.SimplePiece << 6) + posInt(pos),
        (pieceInts(piece) << 4) + (checkInts(check) << 2) + (boolInt(capture) << 1)
      )

    def drop(piece: String, pos: String, check: String) =
      List(
        (MoveType.SimplePiece << 6) + posInt(pos),
        (dropPieceInts(piece) << 4) + (checkInts(check) << 2) + 1
      )

    def castling(str: String, check: String) =
      List(
        MoveType.SimplePiece << 6,
        (pieceInts(str) << 4) + (checkInts(check) << 2)
      )

    def fullPawn(file: Option[String], pos: String, check: String, promotion: Option[String]) =
      List(
        (MoveType.FullPawn << 6) + posInt(pos),
        (shiftOptionInt(file, pos) << 6) + (checkInts(check) << 4) + (promotionInts(promotion | "") << 1)
      )

    def fullPiece(piece: String, orig: String, pos: String, capture: String, check: String) =
      List(
        (MoveType.FullPiece << 6) + posInt(pos),
        (pieceInts(piece) << 4) + (checkInts(check) << 2) + (boolInt(capture) << 1),
        (disambTypeInt(orig) << 6) + disambiguationInt(orig)
      )

    def disambTypeInt(orig: String): Int =
      if (orig.length > 1) 2
      else if (orig.head.toInt < 97) 1
      else 0

    def disambiguationInt(orig: String): Int =
      if (orig.length > 1) posInt(orig)
      else if (orig.head.toInt < 97) rankInt(orig.head)
      else fileInt(orig.head)

    def boolInt(s: String): Int  = if (s.nonEmpty) 1 else 0
    def boolInt(b: Boolean): Int = if (b) 1 else 0

    def posInt(pos: String): Int    = posInt(fileInt(pos.head), rankInt(pos(1)))
    def posInt(x: Int, y: Int): Int = (x << 3) + y

    def fileInt(c: Char): Int = c.toInt - 97
    def rankInt(c: Char): Int = c.toInt - 49

    def shiftOptionInt(fileOption: Option[String], pos: String): Int =
      fileOption.fold(0) { file =>
        if (file.head < pos.head) 1 else 2
      }

    val pieceR       = "([KQRNBL])"
    val fileR        = "(?:([a-h])x)?"
    val posR         = "([a-h][1-9])"
    val captureR     = "(x?)"
    val checkR       = "([\\+#]?)"
    val promotionR   = "(?:\\=?([QRNBK]))?"
    val origR        = "([a-h]?[1-8]?)".r
    val SimplePieceR = s"^$pieceR$captureR$posR$checkR$$".r
    val FullPawnR    = s"^$fileR$posR$promotionR$checkR$$".r
    val CastlingR    = s"^(O-O|O-O-O)$checkR$$".r
    val FullPieceR   = s"^$pieceR$origR$captureR$posR$checkR$$".r
    val DropR        = s"^([QRNBP])@$posR$checkR$$".r
  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String    = "%08d" format (b.toBinaryString.toInt)
}
