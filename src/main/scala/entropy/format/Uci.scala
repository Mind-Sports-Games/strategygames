package strategygames.entropy.format
import strategygames.entropy._
import scala.annotation.nowarn

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
      dest: Pos
  ) extends Uci {

    def keys       = s"${orig.key}${dest.key}"
    def lilaUci    = keys
    def fairySfUci = keys
    def fishnetUci = fairySfUci
    def uci        = keys

    def piotrStr = s"${orig.piotrStr}${dest.piotrStr}"
    def piotr    = piotrStr

    def origDest = Some(orig -> dest)

    def apply(situation: Situation) = situation.move(orig, dest)
  }

  object Move {

    def apply(move: String): Option[Move] = move match {
      case moveR(orig, dest) =>
        (Pos.fromKey(orig), Pos.fromKey(dest)) match {
          case (Some(orig), Some(dest)) => Some(Move(orig, dest))
          case _                        => None
        }
      case _                 => None
    }

    def piotr(move: String): Option[Move] =
      for {
        orig <- move.headOption flatMap Pos.piotr
        dest <- move lift 1 flatMap Pos.piotr
      } yield Move(orig, dest)

    def fromStrings(origS: String, destS: String) =
      for {
        orig <- Pos.fromKey(origS)
        dest <- Pos.fromKey(destS)
      } yield Move(orig, dest)

    val moveR = s"^${Pos.posR}${Pos.posR}$$".r

  }

  case class Drop(role: Role, pos: Pos) extends Uci {

    def lilaUci    = s"${role.pgn}@${pos.key}"
    def fairySfUci = lilaUci
    def fishnetUci = fairySfUci
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

    val dropR = s"^${Role.roleR}@${Pos.posR}$$".r

  }

  case class Pass() extends Uci {

    def lilaUci    = "pass"
    def fairySfUci = lilaUci
    def fishnetUci = fairySfUci
    def uci        = lilaUci

    def piotr = "pass"

    def origDest = None

    def apply(situation: Situation) = situation.pass()
  }

  object Pass {

    val passR = s"^pass$$".r

  }

  // carries the colour drawn, so that a replayed game draws what the original game drew
  case class DrawCounter(role: Role) extends Uci {

    def lilaUci    = s"draw-${role.pgn}"
    def fairySfUci = lilaUci
    def fishnetUci = fairySfUci
    def uci        = lilaUci

    def piotr = lilaUci

    def origDest = None

    def apply(situation: Situation) = situation.drawCounter(role)
  }

  object DrawCounter {

    def fromStrings(roleS: String) =
      Role.allByForsyth.get(roleS.headOption.getOrElse('?')).map(DrawCounter(_))

    val drawCounterR = s"^draw-${Role.roleR}$$".r

  }

  // "I have to draw, but I do not know yet what will come out of the bag"
  case class DoDrawCounter() extends Uci {

    def lilaUci    = "draw"
    def fairySfUci = lilaUci
    def fishnetUci = fairySfUci
    def uci        = lilaUci

    def piotr = "draw"

    def origDest = None

    def apply(situation: Situation) =
      Validated.invalid("DoDrawCounter is a request to draw, not an action")
  }

  object DoDrawCounter {

    val doDrawCounterR = s"^draw$$".r

  }

  case class WithSan(uci: Uci, san: String)

  def apply(move: strategygames.entropy.Move) = Uci.Move(move.orig, move.dest)

  def apply(drop: strategygames.entropy.Drop) = Uci.Drop(drop.piece.role, drop.pos)

  def apply(@nowarn pass: strategygames.entropy.Pass) = Uci.Pass()

  def apply(dc: strategygames.entropy.DrawCounter) = Uci.DrawCounter(dc.role)

  def apply(move: String): Option[Uci] =
    move match {
      case Move.moveR(orig, dest)         =>
        (Pos.fromKey(orig), Pos.fromKey(dest)) match {
          case (Some(orig), Some(dest)) => Uci.Move(orig, dest).some
          case _                        => sys.error(s"Cannot apply uci move: ${move}")
        }
      case Drop.dropR(role, dest)         =>
        (Role.allByPgn.get(role.charAt(0)), Pos.fromKey(dest)) match {
          case (Some(role), Some(dest)) => Uci.Drop(role, dest).some
          case _                        => sys.error(s"Cannot apply uci drop: ${move}")
        }
      case DrawCounter.drawCounterR(role) =>
        Role.allByForsyth.get(role.charAt(0)) match {
          case Some(role) => Uci.DrawCounter(role).some
          case _          => sys.error(s"Cannot apply uci draw: ${move}")
        }
      case DoDrawCounter.doDrawCounterR() => Uci.DoDrawCounter().some
      case Pass.passR()                   => Uci.Pass().some
      case _                              => sys.error(s"Cannot apply uci: ${move}")
    }

  def piotr(move: String): Option[Uci] =
    if (move == "pass") Uci.Pass().some
    else if (move == "draw") Uci.DoDrawCounter().some
    else if (move.take(5) == "draw-") Role.allByForsyth.get(move.charAt(5)).map(Uci.DrawCounter(_))
    else if (move.lift(1).contains('@'))
      for {
        role <- move.headOption flatMap Role.allByPgn.get
        pos  <- move lift 2 flatMap Pos.piotr
      } yield Uci.Drop(role, pos)
    else
      for {
        orig <- move.headOption flatMap Pos.piotr
        dest <- move lift 1 flatMap Pos.piotr
      } yield Uci.Move(orig, dest)

  def readList(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(apply(_)).sequence

  def writeList(moves: List[Uci]): String =
    moves.map(_.uci) mkString " "

  def readListPiotr(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(piotr).sequence

  def writeListPiotr(moves: List[Uci]): String =
    moves.map(_.piotr) mkString " "
}
