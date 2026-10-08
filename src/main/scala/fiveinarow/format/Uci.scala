package strategygames.fiveinarow.format

import strategygames.fiveinarow._

import cats.data.Validated
import cats.implicits._

sealed trait Uci {

  def uci: String
  def piotr: String

  def origDest: Option[(Pos, Pos)]

  def apply(situation: Situation): Validated[String, Action]
}

object Uci {

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

  case class Swap() extends Uci {

    def lilaUci    = "swap"
    def fairySfUci = lilaUci
    def fishnetUci = fairySfUci
    def uci        = lilaUci

    def piotr = lilaUci

    def origDest = None

    def apply(situation: Situation) = situation.swap()
  }

  object Swap {

    val swapR = s"^swap$$".r

  }

  case class Swap2() extends Uci {

    def lilaUci    = "swap2"
    def fairySfUci = lilaUci
    def fishnetUci = fairySfUci
    def uci        = lilaUci

    def piotr = lilaUci

    def origDest = None

    def apply(situation: Situation) = situation.swap2()
  }

  object Swap2 {

    val swap2R = s"^swap2$$".r

  }

  case class WithSan(uci: Uci, san: String)

  def apply(drop: strategygames.fiveinarow.Drop) = Uci.Drop(drop.piece.role, drop.pos)

  def apply(move: String): Option[Uci] =
    move match {
      case Drop.dropR(role, dest) =>
        (Role.allByPgn.get(role.charAt(0)), Pos.fromKey(dest)) match {
          case (Some(role), Some(dest)) => Uci.Drop(role, dest).some
          case _                        => None
        }
      case Swap.swapR()           => Uci.Swap().some
      case Swap2.swap2R()         => Uci.Swap2().some
      case _                      => None
    }

  def piotr(move: String): Option[Uci] =
    if (move == "swap") Uci.Swap().some
    else if (move == "swap2") Uci.Swap2().some
    else if (move.lift(1).contains('@'))
      for {
        role <- move.headOption flatMap Role.allByPgn.get
        pos  <- move lift 2 flatMap Pos.piotr
      } yield Uci.Drop(role, pos)
    else None

  def readList(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(apply(_)).sequence

  def writeList(moves: List[Uci]): String =
    moves.map(_.uci) mkString " "

  def readListPiotr(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(piotr).sequence

  def writeListPiotr(moves: List[Uci]): String =
    moves.map(_.piotr) mkString " "
}
