package strategygames.dameo
package format.pdn

import scala.util.Try
import strategygames.ActionStrs
import strategygames.dameo.format.Uci

object Binary {

  def writeMove(m: String)             = Try(Writer ply m)
  def writeMoves(ms: Iterable[String]) = Try(Writer plies ms)

  def writeActionStrs(as: ActionStrs) = Try(Writer actionStrs as)

  def readActionStrs(bs: List[Byte])          = Try(Reader actionStrs bs)
  def readActionStrs(bs: List[Byte], nb: Int) = Try(Reader.actionStrs(bs, nb))

  private object ActionType {
    val Move = 0
  }

  private val headerBits = 1
  private val tailBits   = 8 - headerBits

  private object Reader {

    // If changing this, consider changing other gamelogics and also lila game maxPlies
    private val maxPlies = 1000

    def actionStrs(bs: List[Byte]): ActionStrs          = actionStrs(bs, maxPlies)
    def actionStrs(bs: List[Byte], nb: Int): ActionStrs = toActionStrs(intPlies(bs map toInt, nb))

    def toActionStrs(plies: List[String]): ActionStrs =
      plies.foldLeft(List.empty[List[String]]) { (acc, ply) =>
        if (acc.isEmpty) List(List(ply))
        else {
          val lastTurn = acc.last
          (lastTurn.last, ply) match {
            case (Uci.Move.moveR(_, lastDst, _), Uci.Move.moveR(nextSrc, _, _)) if lastDst == nextSrc =>
              acc.init :+ (lastTurn :+ ply)
            case _                                                                                    =>
              acc :+ List(ply)
          }
        }
      }

    private def intPlies(bs: List[Int], pliesToGo: Int): List[String] = bs match {
      case _ if pliesToGo < 0                                   => Nil
      case Nil                                                  => Nil
      case b1 :: b2 :: rest if headerInt(b1) == ActionType.Move =>
        if (pliesToGo == 0) Nil
        else moveUci(b1, b2) :: intPlies(rest, pliesToGo - 1)
      case x                                                    => !!(x map showByte mkString ",")
    }

    // 1 movetype
    // 7 srcPos
    // ----
    // 1 promotion
    // 7 dstPos
    def moveUci(b1: Int, b2: Int): String = s"${posFromInt(b1)}${posFromInt(b2)}${promotionFromInt(b2)}"

    def posFromInt(b: Int): String       = Pos(right(b, tailBits)).get.toString()
    def promotionFromInt(b: Int): String = if (headerInt(b) == 0) "" else "K"

    private def headerInt(i: Int) = i >> tailBits

    private def right(i: Int, x: Int): Int = i & lengthMasks(x)
    private val lengthMasks                =
      Map(1 -> 0x01, 2 -> 0x03, 3 -> 0x07, 4 -> 0x0f, 5 -> 0x1f, 6 -> 0x3f, 7 -> 0x7f, 8 -> 0xff)
    private def !!(msg: String)            = throw new Exception("Binary reader failed: " + msg)

  }

  private object Writer {

    def ply(str: String): List[Byte] = (str match {
      case Uci.Move.moveR(src, dst, prom) => moveUci(src, dst, prom)
      case _                              => sys.error(s"Invalid move to write: ${str}")
    }) map (_.toByte)

    def plies(strs: Iterable[String]): Array[Byte] = strs.flatMap(ply).to(Array)

    def actionStrs(strs: ActionStrs): Array[Byte] = strs.flatten.flatMap(ply).to(Array)

    def moveUci(src: String, dst: String, promotion: String) = List(
      (ActionType.Move << tailBits) + Pos.fromKey(src).get.index,
      ((if (promotion.size > 0) 1 else 0) << tailBits) + Pos.fromKey(dst).get.index
    )

  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String    = "%08d" format (b.toBinaryString.toInt)

}
