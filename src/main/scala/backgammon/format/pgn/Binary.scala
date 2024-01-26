package strategygames.backgammon
package format.pgn

import strategygames.backgammon.format.Uci
import strategygames.ActionStrs

import scala.util.Try

object Binary {

  // writeMove only used in tests
  def writeMove(m: String)             = Try(Writer.ply(m))
  def writeMoves(ms: Iterable[String]) = Try(Writer.plies(ms))

  def writeActionStrs(ms: ActionStrs) = Try(Writer.actionStrs(ms))

  def readActionStrs(bs: List[Byte])          = Try(Reader actionStrs bs)
  def readActionStrs(bs: List[Byte], nb: Int) = Try(Reader.actionStrs(bs, nb))

  // No MoveType implemented for Delimiter/Multimove

  private object ActionType {
    val DiceRoll = 0 // needs 6 bits, 3 for each dice
    val OnePos   = 1 // needs 6 bits, 1 for Drop/PickUp, 5 for Pos, 1 for Player?
    val TwoPos   = 2 // needs 10 bits, handles Move, 5 for each Pos
    val Boolean  = 3 // needs 3 bits to determine type as it handles:
    //                  Pass, Confirm, Undo, Double Offer, Double Accept
  }

  private def right(i: Int, x: Int): Int          = i & lengthMasks(x)
  private def subset(i: Int, a: Int, b: Int): Int = right(right(i, a) >> b, b)
  private val lengthMasks                         =
    Map(1 -> 0x01, 2 -> 0x03, 3 -> 0x07, 4 -> 0x0f, 5 -> 0x1f, 6 -> 0x3f, 7 -> 0x7f, 8 -> 0xff)

  private object Reader {

    private val maxPlies = 600

    def actionStrs(bs: List[Byte]): ActionStrs          = actionStrs(bs, maxPlies)
    def actionStrs(bs: List[Byte], nb: Int): ActionStrs = toActionStrs(intPlies(bs map toInt, nb))

    // currently abusing the fact that a diceRoll signifies a new turn
    // will change this when confirm action is in place
    def toActionStrs(plies: List[String]): ActionStrs = plies
      .map { ply =>
        ply match {
          case Uci.DiceRoll.diceRollR(_) => s"#${ply}"
          case _                         => s",${ply}"
        }
      }
      .mkString
      .split("#")
      .drop(1)
      .map(_.split(",").toList)
      .toList

    def intPlies(bs: List[Int], pliesToGo: Int): List[String] =
      bs match {
        case _ if pliesToGo <= 0                                      => Nil
        case Nil                                                      => Nil
        case (b1 :: rest) if headerBit(b1) == ActionType.DiceRoll     =>
          rollDiceUci(b1) :: intPlies(rest, pliesToGo - 1)
        case (b1 :: rest) if headerBit(b1) == ActionType.OnePos       =>
          onePosUci(b1) :: intPlies(rest, pliesToGo - 1)
        case (b1 :: b2 :: rest) if headerBit(b1) == ActionType.TwoPos =>
          twoPosUci(b1, b2) :: intPlies(rest, pliesToGo - 1)
        case (b1 :: rest) if headerBit(b1) == ActionType.Boolean      =>
          booleanUci(b1) :: intPlies(rest, pliesToGo - 1)
        case x                                                        =>
          !!(x map showByte mkString ",")
      }

    // 2 action type
    // 3 first dice (1-6)
    // 3 second dice (1-6)
    def rollDiceUci(b1: Int): String = List(subset(b1, 6, 3), right(b1, 3)).mkString("/")

    // 2 action type
    // 1 drop (0) or pickup (1)
    // 1 player (P1: 0, P2: 1)
    // 1 offset
    // 3 pos (1-6)
    def onePosUci(b1: Int): String =
      if (subset(b1, 5, 1) == 1) {
        // pickup
        sys.error("Pickup not encoded yet")
      } else {
        // drop
        val role   = if (subset(b1, 4, 1) == 1) "S" else "s"
        val offset = if (subset(b1, 3, 1) == 1) 0 else 18
        val pos    = Pos(right(b1, 3) + offset).get.toString()
        s"${role}@${pos}"
      }

    // 2 action type
    // 6 from pos (needs 5)
    // ---------
    // 8 to pos (needs 5)
    def twoPosUci(b1: Int, b2: Int): String =
      s"${posFromInt(right(b1, 6))}${posFromInt(b2)}"

    // 2 action type
    // 6 boolean type: 0 confirm, 1 pass. Others uncoded yet
    def booleanUci(b1: Int): String = right(b1, 6) match {
      case 0 => "lock"
      case 1 => "pass"
      case _ => sys.error("uncoded boolean type")
    }

    def posFromInt(b1: Int): String = Pos(right(b1, 6)).get.toString()

    private def headerBit(i: Int) = i >> 6

    private def !!(msg: String) = throw new Exception("Binary reader failed: " + msg)
  }

  private object Writer {

    def ply(str: String): List[Byte] =
      (str match {
        case Uci.DiceRoll.diceRollR(dr) => diceRollUci(dr)
        case Uci.Move.moveR(orig, dest) => moveUci(orig, dest)
        // case Uci.Drop.dropR(_, dst) => dropUci(dst)
        // case Uci.Pass.passR()                     => passUci
        // case Uci.SelectSquares.selectSquaresR(ss) => selectSquaresUci(ss)
        case _                          => sys.error(s"Invalid action to write: ${str}")
      }) map (_.toByte)

    def plies(strs: Iterable[String]): Array[Byte] =
      strs.toList.flatMap(ply).to(Array)

    // can flatten because this GameLogic has a clear definition of an end of a turn in the actions
    def actionStrs(strs: ActionStrs): Array[Byte] = plies(strs.flatten)

    // def passUci = List(headerBit(MoveType.Pass))

    def diceRollUci(dr: String) = {
      val dice = Uci.DiceRoll.fromStrings(dr).dice
      List(
        headerBit(ActionType.DiceRoll) + (dice(0) << 3) + dice(1)
      )
    }

    def moveUci(orig: String, dest: String) = List(
      (headerBit(ActionType.TwoPos)) + Pos.fromKey(orig).get.index,
      Pos.fromKey(dest).get.index
    )

    // def dropUci(dst: String) = List(
    //  (headerBit(ActionType.OnePos)) + (Pos.fromKey(dst).get.index >> 8),
    //  right(Pos.fromKey(dst).get.index, 8)
    // )

    private def headerBit(i: Int) = i << 6

  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String    = "%08d" format (b.toBinaryString.toInt)
}
