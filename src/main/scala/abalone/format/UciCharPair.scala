package strategygames.abalone.format

import strategygames.abalone._
import strategygames.format.{UciCharPair => stratUciCharPair}

//think this is only used for analysis/puzzles?
object UciCharPair {

  import implementation._

  def apply(uci: Uci): stratUciCharPair =
    uci match {
      case Uci.Move(orig, dest) => stratUciCharPair(toChar(orig), toChar(dest))
    }

  private[format] object implementation {
    val charShift = 35 // Start at Char(35) == '#'
    val voidChar = 33.toChar // '!'. We skipped Char(34) == '"'.

    val pos2charMap: Map[Pos, Char] = Piotr.posToPiotr.keys
      .map { pos =>
        pos -> (pos.hashCode + charShift).toChar
      }
      .to(Map)

    def toChar(pos: Pos) = pos2charMap.getOrElse(pos, voidChar)

    def toChar(file: Int, prom: PromotableRole) = (file -> prom, voidChar)

    // copied from chess (minus !King filter), im sure this just gives '?' for all roles
    val dropRole2charMap: Map[Role, Char] =
      Role.all.zipWithIndex
        .map { case (role, index) =>
          role -> (charShift + pos2charMap.size + index).toChar
        }
        .to(Map)
  }
}