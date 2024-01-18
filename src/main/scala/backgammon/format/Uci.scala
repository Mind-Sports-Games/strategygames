package strategygames.backgammon.format
import strategygames.backgammon._

import strategygames.GameFamily

import cats.data.Validated
import cats.implicits._

sealed trait Uci {

  def uci: String
  def piotr: String

  def origDest: Option[(Pos, Pos)]

  def apply(situation: Situation): Validated[String, Action]
}

object Uci {

  case class Move(
      orig: Pos,
      dest: Pos,
      promotion: Option[PromotableRole] = None
  ) extends Uci {

    def keys = orig.key + dest.key
    def uci  = keys + promotionString

    def keysPiotr = orig.piotrStr + dest.piotrStr
    def piotr     = keysPiotr + promotionString

    def promotionString = promotion.fold("")(_.forsyth.toString)

    def origDest = Some(orig -> dest)

    def apply(situation: Situation) = situation.move(orig, dest, promotion)
  }

  object Move {

    def apply(move: String): Option[Move] =
      move match {
        case moveR(orig, dest) =>
          (
            Pos.fromKey(orig),
            Pos.fromKey(dest)
          ) match {
            case (Some(orig), Some(dest)) => {
              Move(
                orig = orig,
                dest = dest
              ).some
            }
            case _                           => None
          }
        case _                            => None
      }

    def piotr(move: String) =
      for {
        orig <- move.headOption flatMap Pos.piotr
        dest <- move lift 1 flatMap Pos.piotr
      } yield Move(orig, dest, promotion = None)

    def fromStrings(gf: GameFamily, origS: String, destS: String, promS: Option[String]) =
      for {
        orig     <- Pos.fromKey(origS)
        dest     <- Pos.fromKey(destS)
        promotion = None
      } yield Move(orig, dest, promotion)

    val moveR = s"^${Pos.posR}${Pos.posR}$$".r
  }

  case class Drop(role: Role, pos: Pos) extends Uci {

    def lilaUci    = s"${role.pgn}@${pos.key}"
    def fishnetUci = lilaUci
    def uci        = lilaUci

    def piotr = s"${role.pgn}@${pos.piotrStr}"

    def origDest = Some(pos -> pos)

    def apply(situation: Situation) = situation.drop(role, pos)
  }

  object Drop {

    def fromStrings(roleS: String, posS: String) =
      for {
        role <- Role.allByName get roleS
        pos  <- Pos.fromKey(posS)
      } yield Drop(role, pos)

    val dropR = s"^${Role.roleRr}@${Pos.posR}$$".r

  }

  case class DiceRoll(dice: List[Int]) extends Uci {

    def uci = dice.mkString("|")

    def piotr = uci

    def origDest = None

    def apply(situation: Situation) = situation.diceRoll(dice)
  }

  object DiceRoll {

    def fromStrings(dice: String) = DiceRoll(dice.split('|').flatMap(_.toIntOption).toList)

    val diceR     = "([1-6])"
    val diceRollR = s"^${diceR}\\|${diceR}$$".r

  }

  // this is a stub Uci case class that doesn't marry up to an Action type
  // this stub class says i need to do a roll, i don't know the dice i have rolled
  // its used by lila but not internally by strategygames
  case class DoRoll() extends Uci {

    def uci = "roll"

    def piotr = uci

    def origDest = None

    def apply(situation: Situation) = sys.error("Cannot apply a DoRoll")

  }

  case class WithSan(uci: Uci, san: String)

  def apply(move: strategygames.backgammon.Move) = Uci.Move(move.orig, move.dest)

  def apply(drop: strategygames.backgammon.Drop) = Uci.Drop(drop.piece.role, drop.pos)

  def apply(diceRoll: strategygames.backgammon.DiceRoll) = Uci.DiceRoll(diceRoll.dice)

  def apply(action: String): Option[Uci] =
    if (action lift 1 contains '@') for {
      role <- action.headOption flatMap Role.allByPgn.get
      pos  <- Pos.fromKey(action.slice(2, 4))
    } yield Uci.Drop(role, pos)
    else if (action.contains('|')) Some(Uci.DiceRoll.fromStrings(action))
    else Uci.Move(action)

  def piotr(action: String): Option[Uci] =
    if (action lift 1 contains '@') for {
      role <- action.headOption flatMap Role.allByPgn.get
      pos  <- action lift 2 flatMap Pos.piotr
    } yield Uci.Drop(role, pos)
    else if (action.contains('|')) Some(Uci.DiceRoll.fromStrings(action))
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
