package strategygames.yonkoma.format
import strategygames.yonkoma._
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

  // the skipped turn of a player who has no legal move
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

  case class WithSan(uci: Uci, san: String)

  def apply(move: strategygames.yonkoma.Move) = Uci.Move(move.orig, move.dest)

  def apply(@nowarn pass: strategygames.yonkoma.Pass) = Uci.Pass()

  def apply(move: String): Option[Uci] =
    move match {
      case Move.moveR(orig, dest) =>
        (Pos.fromKey(orig), Pos.fromKey(dest)) match {
          case (Some(orig), Some(dest)) => Uci.Move(orig, dest).some
          case _                        => sys.error(s"Cannot apply uci move: ${move}")
        }
      case Pass.passR()           => Uci.Pass().some
      case _                      => sys.error(s"Cannot apply uci: ${move}")
    }

  def piotr(move: String): Option[Uci] =
    if (move == "pass") Uci.Pass().some
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
