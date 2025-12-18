package strategygames.backgammon.format
import strategygames.backgammon._

//think this is only used for analysis/puzzles?
object UciCharPair {

  import strategygames.format.{ UciCharPair => stratUciCharPair }
  import implementation._

  def apply(uci: Uci): stratUciCharPair =
    uci match {
      case Uci.Move(orig, dest, _) => stratUciCharPair(toChar(orig), toChar(dest))
      case Uci.Drop(role, pos, _)  =>
        stratUciCharPair(
          toChar(pos),
          dropRole2charMap.getOrElse(role, voidChar)
        )
      case Uci.Lift(pos)           =>
        stratUciCharPair(
          dropRole2charMap.getOrElse(Role.defaultRole, voidChar),
          toChar(pos)
        )
      case Uci.DiceRoll(dice)      =>
        stratUciCharPair(
          dice2char(dice(0)),
          dice2char(dice(1))
        )
      case Uci.Undo()              => uciCharToCharPair(toChar(Pos.C1))
      case Uci.DoRoll()            => uciCharToCharPair(toChar(Pos.B1))
      case Uci.EndTurn()           => uciCharToCharPair(toChar(Pos.A1))
      // OfferDouble => L1, AcceptDouble => K1, RejectDouble => J1
      case Uci.CubeAction(ci)      => uciCharToCharPair(toChar(Pos(ci.index).getOrElse(Pos.D1)))
      case null                    => sys.error(s"Not implemented UciCharPair for $uci")
    }

  private def uciCharToCharPair(c: Char) = stratUciCharPair(c, c)

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

    def dice2char(dice: Int): Char =
      (charShift + pos2charMap.size + dropRole2charMap.size + dice).toChar
  }
}
