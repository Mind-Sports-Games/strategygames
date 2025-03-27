package strategygames.abalone.format

import cats.data.Validated
import cats.implicits._
import strategygames.abalone.{Pos, Situation}

sealed trait Uci {
  def uci: String

  def piotr: String

  def origDest: (Pos, Pos)

  def apply(situation: Situation): Validated[String, strategygames.abalone.Move]
}

object Uci {
  case class Move(orig: Pos, dest: Pos) extends Uci {
    def keys: String = orig.key + dest.key

    override def uci = keys

    def keysPiotr = orig.piotrStr + dest.piotrStr

    override def piotr = keysPiotr

    override def origDest = orig -> dest

    override def apply(situation: Situation) = situation.move(orig, dest)
  }

  object Move {
    def apply(move: String): Option[Move] =
      move match {
        case moveR(orig0, orig1, dest0, dest1) =>
          (
            Pos.fromKey(orig0 + orig1),
            Pos.fromKey(dest0 + dest1)
          ) match {
            case (Some(orig), Some(dest)) => Move(orig = orig, dest = dest).some
            case _                        => None
          }
        case _                 => None
      }

    def piotr(move: String) =
      for {
        orig <- move.headOption flatMap Pos.piotr
        dest <- move lift 1 flatMap Pos.piotr
      } yield Move(orig, dest)

    def fromStrings(origS: String, destS: String) =
      for {
        orig <- Pos.fromKey(origS)
        dest <- Pos.fromKey(destS)
      } yield Move(orig, dest)

    val moveR = s"^${Pos.re}${Pos.re}".r
  }

  case class WithSan(uci: Uci, san: String)

  def apply(move: strategygames.abalone.Move) = Uci.Move(move.orig, move.dest)

  def apply(move: String) = Uci.Move(move)

  def piotr(move: String): Option[Uci] = Uci.Move.piotr(move)

  def readList(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(apply(_)).sequence

  def writeList(moves: List[Uci]): String =
    moves.map(_.uci) mkString " "

  def readListPiotr(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(piotr).sequence

  def writeListPiotr(moves: List[Uci]): String =
    moves.map(_.piotr) mkString " "
}
