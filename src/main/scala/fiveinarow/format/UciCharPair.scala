package strategygames.fiveinarow.format

import strategygames.fiveinarow._

object UciCharPair {

  import strategygames.format.{ UciCharPair => stratUciCharPair }
  import implementation._

  def apply(uci: Uci): stratUciCharPair =
    uci match {
      case Uci.Drop(role, pos) =>
        stratUciCharPair(toChar(pos), dropRole2charMap.getOrElse(role, voidChar))
      case Uci.Swap()          => stratUciCharPair(voidChar, swapChar)
      case Uci.Swap2()         => stratUciCharPair(voidChar, swap2Char)
    }

  private[format] object implementation {

    val charShift = 35        // Start at Char(35) == '#'
    val voidChar  = 33.toChar // '!'. We skipped Char(34) == '"'.

    val pos2charMap: Map[Pos, Char] = Pos.all
      .map { pos =>
        pos -> (pos.index + charShift).toChar
      }
      .to(Map)

    def toChar(pos: Pos) = pos2charMap.getOrElse(pos, voidChar)

    val dropRole2charMap: Map[Role, Char] =
      Role.all.zipWithIndex
        .map { case (role, index) =>
          role -> (charShift + pos2charMap.size + index).toChar
        }
        .to(Map)

    val swapChar: Char  = (charShift + pos2charMap.size + Role.all.size).toChar
    val swap2Char: Char = (swapChar + 1).toChar

  }
}
