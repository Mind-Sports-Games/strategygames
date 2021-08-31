package strategygames.draughts
package format

object UciCharPair {

  import implementation._
  import strategygames.format.{ UciCharPair => stratUciCharPair }

  def apply(uci: Uci): stratUciCharPair = stratUciCharPair(toChar(uci.origDest._1), toChar(uci.origDest._2))
  def apply(uci: Uci, ambiguity: Int): stratUciCharPair =
    stratUciCharPair(toChar(uci.origDest._1), ambiguity2charMap.getOrElse(ambiguity, voidChar))
  def apply(orig: Char, ambiguity: Int): stratUciCharPair =
    stratUciCharPair(orig, ambiguity2charMap.getOrElse(ambiguity, voidChar))

  def combine(uci1: Uci, uci2: Uci): stratUciCharPair =
    stratUciCharPair(toChar(uci1.origDest._1), toChar(uci2.origDest._2))

  private[format] object implementation {

    type File = Int

    val charShift = 35        // Start at Char(35) == '#'
    val voidChar  = 33.toChar // '!'. We skipped Char(34) == '"'.

    val pos2charMap: Map[Pos, Char] = Board.BoardSize.max.all
      .map { pos =>
        pos -> (pos.hashCode + charShift).toChar
      }
      .to(Map)

    def toChar(pos: Pos) = pos2charMap.getOrElse(pos, voidChar)

    /** Allow for 50 ambiguities per destination, should be enough
      */
    val ambiguity2charMap: Map[Int, Char] = (for {
      ambNr <- 1 to 50
    } yield ambNr -> (charShift + pos2charMap.size + ambNr).toChar).to(Map)

  }
}
