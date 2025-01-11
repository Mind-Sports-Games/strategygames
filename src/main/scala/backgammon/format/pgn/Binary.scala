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

  private object ActionType {
    val DiceRoll = 0 // needs 6 bits, 3 for each dice
    val OnePos   = 1 // needs 5 bits, 1 for Drop/Lift, 1 for Player, 3 for pos offset
    val TwoPos   = 2 // needs 10 bits, handles Move, 5 for each Pos
    val Boolean  = 3 // needs 2-3 bits to determine type as it handles:
    //                  EndTurn, Double Offer, Double Accept, Double Reject
  }

  private def right(i: Int, x: Int): Int          = i & lengthMasks(x)
  // subset and bitAt have been added for backgammon be careful when using these
  // havent tested outside the cases used for
  private def subset(i: Int, a: Int, b: Int): Int = right(right(i, a) >> b, b)
  private def bitAt(i: Int, a: Int, b: Int): Int  = (i - a) >> b
  private val lengthMasks                         =
    Map(1 -> 0x01, 2 -> 0x03, 3 -> 0x07, 4 -> 0x0f, 5 -> 0x1f, 6 -> 0x3f, 7 -> 0x7f, 8 -> 0xff)

  private object Reader {

    // If changing this, consider changing other gamelogics and also lila game maxPlies
    private val maxPlies = 1000

    def actionStrs(bs: List[Byte]): ActionStrs          = actionStrs(bs, maxPlies)
    def actionStrs(bs: List[Byte], nb: Int): ActionStrs = toActionStrs(intPlies(bs map toInt, nb))

    def toActionStrs(plies: List[String]): ActionStrs =
      if (plies.isEmpty) List()
      else
        plies
          .map { ply =>
            ply match {
              case Uci.EndTurn.endTurnR() => s"${ply}#"
              case _                      => s"${ply},"
            }
          }
          .mkString
          .split("#")
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
    // 1 drop (0) or lift (1)
    // 1 player (P1: 0, P2: 1)
    // 3 pos (1-6)
    // as drops can only be into the first 6 or last 6 squares (depending on player)
    // and also lifts can only come from the last 6 or first 6 squares (depending on player)
    // we only need to cater for 6 possible pos, which then get mapped to correct pos
    // depending on player and action
    def onePosUci(b1: Int): String =
      if (bitAt(b1, ActionType.OnePos << 6, 5) == 0) {
        // drop
        val player = bitAt(b1, ActionType.OnePos << 6, 4)
        val pos    = Pos(right(b1, 3) + (if (player == 0) 18 else 0)).get.toString()
        s"${Role.defaultRole.forsyth}@${pos}"
      } else {
        // lift
        val player = bitAt(b1, (ActionType.OnePos << 6) + (1 << 5), 4)
        val pos    = Pos(right(b1, 3) + (if (player == 0) 0 else 18)).get.toString()
        s"^${pos}"
      }

    // 2 action type
    // 6 from pos (needs 5)
    // ---------
    // 8 to pos (needs 5)
    def twoPosUci(b1: Int, b2: Int): String =
      s"${posFromInt(right(b1, 6))}${posFromInt(b2)}"

    // 2 action type
    // 6 boolean type: 0 EndTurn. Others uncoded yet
    def booleanUci(b1: Int): String = right(b1, 6) match {
      case 0 => "endturn"
      case _ => sys.error("uncoded boolean type")
    }

    def posFromInt(b1: Int): String = Pos(right(b1, 6)).get.toString()

    private def headerBit(i: Int) = i >> 6

    private def !!(msg: String) = throw new Exception("Binary reader failed: " + msg)
  }

  private object Writer {

    def ply(str: String): List[Byte] =
      (str match {
        case Uci.DiceRoll.diceRollR(dr)    => diceRollUci(dr)
        case Uci.Move.moveR(orig, dest, _) => moveUci(orig, dest)
        case Uci.Drop.dropR(_, dest, _)    => dropUci(dest)
        case Uci.Lift.liftR(_, orig)       => liftUci(orig)
        case Uci.EndTurn.endTurnR()        => endTurnUci()
        case _                             => sys.error(s"Invalid action to write: ${str}")
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

    def dropUci(dest: String) = {
      val posIndex  = Pos.fromKey(dest).get.index
      val playerBit = if (posIndex < 18) 1 << 4 else 0
      val offset    = if (posIndex < 18) 0 else 18
      List(headerBit(ActionType.OnePos) + playerBit + posIndex - offset)
    }

    def liftUci(orig: String) = {
      val liftBit   = 1 << 5
      val posIndex  = Pos.fromKey(orig).get.index
      val playerBit = if (posIndex < 18) 0 else 1 << 4
      val offset    = if (posIndex < 18) 0 else 18
      List(headerBit(ActionType.OnePos) + liftBit + playerBit + posIndex - offset)
    }

    def endTurnUci() = List((headerBit(ActionType.Boolean)))

    private def headerBit(i: Int) = i << 6

  }

  @inline private def toInt(b: Byte): Int = b & 0xff
  private def showByte(b: Int): String    = "%08d" format (b.toBinaryString.toInt)

}
