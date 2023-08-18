package strategygames.go.format
import strategygames.go._

//think this is only used for analysis/puzzles?
object UciCharPair {

  import strategygames.format.{ UciCharPair => stratUciCharPair }
  import implementation._

  def apply(uci: Uci): stratUciCharPair =
    uci match {
      case Uci.Drop(role, pos)  =>
        stratUciCharPair(
          toChar(pos),
          dropRole2charMap.getOrElse(role, voidChar)
        )
      case Uci.Pass()           => stratUciCharPair(toChar(Pos.A1), toChar(Pos.A1))
      case Uci.SelectSquares(_) => stratUciCharPair(toChar(Pos.A1), toChar(Pos.A1))
      case _                    => sys.error(s"Go does not have moves or promotable roles uci: ${uci}")
    }

  private[format] object implementation {

    val charShift = 35        // Start at Char(35) == '#'
    val voidChar  = 33.toChar // '!'. We skipped Char(34) == '"'.

    val pos2charMap: Map[Pos, Char] = Pos.all
      .map { pos =>
        pos -> (pos.hashCode + charShift).toChar
      }
      .to(Map)

    def toChar(pos: Pos) = pos2charMap.getOrElse(pos, voidChar)

    def toChar(file: File, prom: PromotableRole) = (file -> prom, voidChar)

    // copied from chess (minus !King filter), im sure this just gives '?' for all roles
    val dropRole2charMap: Map[Role, Char] =
      Role.all.zipWithIndex
        .map { case (role, index) =>
          role -> (charShift + pos2charMap.size + index).toChar
        }
        .to(Map)

  }
}
