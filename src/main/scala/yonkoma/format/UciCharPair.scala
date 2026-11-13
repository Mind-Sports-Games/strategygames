package strategygames.yonkoma.format
import strategygames.yonkoma._

object UciCharPair {

  import strategygames.format.{ UciCharPair => stratUciCharPair }
  import implementation._

  def apply(uci: Uci): stratUciCharPair =
    uci match {
      case Uci.Move(orig, dest) => stratUciCharPair(toChar(orig), toChar(dest))
      // a pass is (a1, a1), as in go and entropy; no move starts and ends on one square
      case Uci.Pass()           => stratUciCharPair(toChar(Pos.A1), toChar(Pos.A1))
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

  }
}
