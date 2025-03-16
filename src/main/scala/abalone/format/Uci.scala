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
  case class MMove(orig: Pos, dest: Pos) extends Uci {
    def keys: String = orig.key + dest.key

    override def uci = keys

    def keysPiotr = orig.piotrStr + dest.piotrStr

    override def piotr = keysPiotr

    override def origDest = orig -> dest

    override def apply(situation: Situation) = situation.move(orig, dest)
  }

  object MMove {
    def apply(move: String): Option[MMove] =
      move match {
        case moveR(orig, dest) =>
          (
            Pos.fromKey(orig),
            Pos.fromKey(dest)
          ) match {
            case (Some(orig), Some(dest)) => MMove(orig = orig, dest = dest).some
            case _ => None
          }
        case _ => None
      }

    def piotr(move: String) =
      for {
        orig <- move.headOption flatMap Pos.piotr
        dest <- move lift 1 flatMap Pos.piotr
      } yield MMove(orig, dest)

    def fromStrings(origS: String, destS: String) =
      for {
        orig <- Pos.fromKey(origS)
        dest <- Pos.fromKey(destS)
      } yield MMove(orig, dest)

    val moveR = s"^${Pos.re}${Pos.re}".r
  }

  case class WithSan(uci: Uci, san: String)

  def apply(move: strategygames.abalone.Move) = Uci.MMove(move.orig, move.dest)

  def apply(move: String) = Uci.MMove(move)

  def piotr(move: String): Option[Uci] = Uci.MMove.piotr(move)

  def readList(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(apply(_)).sequence

  def writeList(moves: List[Uci]): String =
    moves.map(_.uci) mkString " "

  def readListPiotr(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(piotr).sequence

  def writeListPiotr(moves: List[Uci]): String =
    moves.map(_.piotr) mkString " "
}