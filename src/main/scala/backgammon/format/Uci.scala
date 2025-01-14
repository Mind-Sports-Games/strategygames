package strategygames.backgammon.format
import strategygames.backgammon._

import scala.annotation.nowarn
import cats.data.Validated
import cats.implicits._

sealed trait Uci {

  def uci: String
  def piotr: String

  def origDest: Option[(Pos, Pos)]

  def undoable: Boolean

  def apply(situation: Situation): Validated[String, Action]
}

object Uci {

  case class Move(
      orig: Pos,
      dest: Pos,
      // same structure as draughts
      capture: Option[List[Pos]] = None
  ) extends Uci {

    def keys = orig.key + dest.key
    def uci  = if (capture.isEmpty) keys else s"${keys}x"

    def keysPiotr = orig.piotrStr + dest.piotrStr
    def piotr     = keysPiotr

    def origDest = Some(orig -> dest)

    def diceUsed = (orig.index - dest.index).abs

    def undoable = true

    def apply(situation: Situation) = situation.move(orig, dest)
  }

  object Move {

    def apply(move: String): Option[Move] =
      move match {
        case moveR(orig, dest, capture) =>
          (
            Pos.fromKey(orig),
            Pos.fromKey(dest),
            capture.length == 1
          ) match {
            case (Some(orig), Some(dest), capture) => {
              Move(
                orig = orig,
                dest = dest,
                capture = if (capture) Some(List(dest)) else None
              ).some
            }
            case _                                 => None
          }
        case _                          => None
      }

    def piotr(move: String) =
      for {
        orig    <- move.headOption.flatMap(Pos.piotr)
        dest    <- move.lift(1).flatMap(Pos.piotr)
        capture <- move.lift(2).nonEmpty.some
      } yield Move(orig, dest, if (capture) Some(List(dest)) else None)

    // this doesnt take in capture in the same way draughts doesnt
    def fromStrings(origS: String, destS: String) =
      for {
        orig <- Pos.fromKey(origS)
        dest <- Pos.fromKey(destS)
      } yield Move(orig, dest)

    val moveR = s"^${Pos.posR}${Pos.posR}([x]?)$$".r
  }

  case class Drop(
      role: Role,
      pos: Pos,
      // same structure as Move, which is copied from draughts
      capture: Option[List[Pos]] = None
  ) extends Uci {

    def lilaUci    = s"${role.pgn}@${pos.key}"
    def fishnetUci = lilaUci
    def uci        = if (capture.isEmpty) lilaUci else s"${lilaUci}x"

    def piotr = s"${role.pgn}@${pos.piotrStr}"

    def origDest = Some(pos -> pos)

    def undoable = true

    def apply(situation: Situation) = situation.drop(role, pos)
  }

  object Drop {

    def apply(drop: String): Option[Drop] =
      for {
        role    <- drop.headOption flatMap Role.allByPgn.get
        pos     <- Pos.fromKey(drop.slice(2, 4))
        capture <- drop.lift(4).nonEmpty.some
      } yield Uci.Drop(role, pos, if (capture) Some(List(pos)) else None)

    def piotr(drop: String) =
      for {
        role    <- drop.headOption flatMap Role.allByPgn.get
        pos     <- drop lift 2 flatMap Pos.piotr
        capture <- drop.lift(3).nonEmpty.some
      } yield Uci.Drop(role, pos, if (capture) Some(List(pos)) else None)

    // this doesnt take in capture in the same way move doesnt
    def fromStrings(roleS: String, posS: String) =
      for {
        role <- Role.allByName get roleS
        pos  <- Pos.fromKey(posS)
      } yield Drop(role, pos)

    val dropR = s"^${Role.roleRr}@${Pos.posR}([x]?)$$".r

  }

  case class Lift(pos: Pos) extends Uci {

    def lilaUci    = s"^${pos.key}"
    def fishnetUci = lilaUci
    def uci        = lilaUci

    def piotr = s"^${pos.piotrStr}"

    def origDest = Some(pos -> pos)

    def undoable = true

    def apply(situation: Situation) = situation.lift(pos)

  }

  object Lift {

    // can parse "^a1" and "1^a1" formats
    def piotr(action: String) =
      action
        .lift(action.length - 1)
        .flatMap(Pos.piotr)
        .map(Lift(_))

    def fromStrings(posS: String) =
      for {
        pos <- Pos.fromKey(posS)
      } yield Lift(pos)

    // allow both "^a1" and "1^a1" formats
    val liftR = s"^([1-6]?)\\^${Pos.posR}$$".r

  }

  case class DiceRoll(dice: List[Int]) extends Uci {

    def uci = dice.mkString("/")

    def piotr = uci

    def origDest = None

    def undoable = false

    def apply(situation: Situation) = situation.diceRoll(dice)
  }

  object DiceRoll {

    // This doesn't return an Option like the other fromStrings functions
    // But its more usable like this and its what Backgammon has been tested with
    def fromStrings(dice: String) = DiceRoll(dice.split('/').flatMap(_.toIntOption).toList)

    val diceRollR = s"^([1-6]\\/[1-6])$$".r

  }

  // this is a stub Uci case class that doesn't marry up to an Action type
  // this stub class says "I need to do a roll, I don't know the dice I have rolled"
  // its used by lila but not internally by strategygames - it never ends up in actionStrs
  case class DoRoll() extends Uci {

    def uci = "roll"

    def piotr = uci

    def origDest = None

    def undoable = false

    def apply(situation: Situation) = sys.error("Cannot apply a DoRoll")

  }

  // another stub Uci case class that doesn't end up in actionStrs
  case class Undo() extends Uci {

    def uci = "undo"

    def piotr = uci

    def origDest = None

    def undoable = false

    def apply(situation: Situation) = situation.undo

  }

  case class EndTurn() extends Uci {

    def uci = "endturn"

    def piotr = uci

    def origDest = None

    def undoable = false

    def apply(situation: Situation) = situation.endTurn
  }

  object EndTurn {

    val endTurnR = s"^endturn$$".r

  }

  case class CubeAction(interaction: CubeInteraction) extends Uci {

    def uci = s"cube${interaction.char}"

    def piotr = uci

    def origDest = None

    def undoable = false

    def apply(situation: Situation) = situation.endTurn // situation.cubeAction(interaction)
  }

  object CubeAction {

    def fromStrings(uci: String): Option[CubeAction] = uci match {
      case "cubeo" => Some(CubeAction(OfferDouble))
      case "cubey" => Some(CubeAction(AcceptDouble))
      case "cuben" => Some(CubeAction(RejectDouble))
      case _       => None
    }

    def fromChar(c: Char): Option[CubeAction]   = fromStrings(s"cube${c}")
    def fromChar(c: String): Option[CubeAction] = fromStrings(s"cube${c}")

    val cubeActionR = s"^cube([oyn])$$".r

  }

  case class WithSan(uci: Uci, san: String)

  def apply(move: strategygames.backgammon.Move) = Uci.Move(move.orig, move.dest, move.captureList)

  def apply(drop: strategygames.backgammon.Drop) = Uci.Drop(drop.piece.role, drop.pos, drop.captureList)

  def apply(lift: strategygames.backgammon.Lift) = Uci.Lift(lift.pos)

  def apply(diceRoll: strategygames.backgammon.DiceRoll) = Uci.DiceRoll(diceRoll.dice)

  def apply(@nowarn undo: strategygames.backgammon.Undo) = Uci.Undo()

  def apply(@nowarn endTurn: strategygames.backgammon.EndTurn) = Uci.EndTurn()

  def apply(cubeAction: strategygames.backgammon.CubeAction) = Uci.CubeAction(cubeAction.interaction)

  def apply(action: String): Option[Uci] =
    if (action lift 1 contains '@') Uci.Drop(action)
    else if (action.contains('^')) Uci.Lift.fromStrings(action.split('^')(1))
    else if (action.contains('/')) Some(Uci.DiceRoll.fromStrings(action))
    else if (action == "roll") Some(Uci.DoRoll())
    else if (action == "undo") Some(Uci.Undo())
    else if (action == "endturn") Some(Uci.EndTurn())
    else if (action.startsWith("cube")) Uci.CubeAction.fromStrings(action)
    else Uci.Move(action)

  def piotr(action: String): Option[Uci] =
    if (action lift 1 contains '@') Uci.Drop.piotr(action)
    else if (action.contains('^')) Uci.Lift.piotr(action)
    else if (action.contains('/')) Some(Uci.DiceRoll.fromStrings(action))
    else if (action == "roll") Some(Uci.DoRoll())
    else if (action == "undo") Some(Uci.Undo())
    else if (action == "endturn") Some(Uci.EndTurn())
    else if (action.startsWith("cube")) Uci.CubeAction.fromStrings(action)
    else Uci.Move.piotr(action)

  def readList(actions: String): Option[List[Uci]] =
    actions.split(' ').toList.map(apply(_)).sequence

  def writeList(actions: List[Uci]): String =
    actions.map(_.uci) mkString " "

  def readListPiotr(actions: String): Option[List[Uci]] =
    actions.split(' ').toList.map(piotr).sequence

  def writeListPiotr(actions: List[Uci]): String =
    actions.map(_.piotr) mkString " "

}
