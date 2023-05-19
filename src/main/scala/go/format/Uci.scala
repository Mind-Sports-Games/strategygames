package strategygames.go.format
import strategygames.go._

import strategygames.GameFamily

import cats.data.Validated
import cats.implicits._

sealed trait Uci {

  def uci: String
  def piotr: String

  def origDest: (Pos, Pos)

  def apply(situation: Situation): Validated[String, Drop]
}

object Uci {

  case class Drop(role: Role, pos: Pos) extends Uci {

    def lilaUci    = s"${role.pgn}@${pos.key}"
    def fairySfUci = lilaUci
    def fishnetUci = fairySfUci // Use the fairySfUci
    def uci        = lilaUci

    def piotr = s"${role.pgn}@${pos.piotrStr}"

    def origDest = pos -> pos

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

  case class WithSan(uci: Uci, san: String)

  def apply(drop: strategygames.go.Drop) = Uci.Drop(drop.piece.role, drop.pos)

  def apply(move: String): Option[Uci] =
    move match {
      case Drop.dropR(role, dest) =>
        (Role.allByPgn.get(role.charAt(0)), Pos.fromKey(dest)) match {
          case (Some(role), Some(dest)) => Uci.Drop(role, dest).some
          case _                        => sys.error(s"Cannot apply uci drop: ${move}")
        }
      case _                      => sys.error(s"Cannot apply uci: ${move}")
    }

  def piotr(move: String): Option[Uci] = {
    for {
      role <- move.headOption flatMap Role.allByPgn.get
      pos  <- move lift 2 flatMap Pos.piotr
    } yield Uci.Drop(role, pos)
  }

  def readList(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(apply(_)).sequence

  def writeList(moves: List[Uci]): String =
    moves.map(_.uci) mkString " "

  def readListPiotr(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(piotr).sequence

  def writeListPiotr(moves: List[Uci]): String =
    moves.map(_.piotr) mkString " "
}
