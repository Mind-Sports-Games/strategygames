package abalone.format

import abalone.SSituation
import abalone.util.geometry.Cell
import cats.data.Validated
import cats.implicits._

sealed trait UUci {
  def uci: String

  def piotr: String

  def origDest: (Cell, Cell)

  def apply(situation: SSituation): Validated[String, abalone.MMove]
}

object UUci {
  case class MMove(orig: Cell, dest: Cell) extends UUci {
    def keys: String = orig.key + dest.key

    override def uci = keys

    def keysPiotr = orig.piotrStr + dest.piotrStr

    override def piotr = keysPiotr

    override def origDest = orig -> dest

    override def apply(situation: SSituation) = situation.move(orig, dest)
  }

  object MMove {
    def apply(move: String): Option[MMove] =
      move match {
        case moveR(orig, dest) =>
          (
            Cell.fromKey(orig),
            Cell.fromKey(dest)
          ) match {
            case (Some(orig), Some(dest)) => MMove(orig = orig, dest = dest).some
            case _ => None
          }
        case _ => None
      }

    def piotr(move: String) =
      for {
        orig <- move.headOption flatMap Cell.piotr
        dest <- move lift 1 flatMap Cell.piotr
      } yield MMove(orig, dest)

    def fromStrings(origS: String, destS: String) =
      for {
        orig <- Cell.fromKey(origS)
        dest <- Cell.fromKey(destS)
      } yield MMove(orig, dest)

    val moveR = s"^${Cell.re}${Cell.re}".r
  }

  case class WithSan(uci: UUci, san: String)

  def apply(move: abalone.MMove) = UUci.MMove(move.orig, move.dest)

  def apply(move: String) = UUci.MMove(move)

  def piotr(move: String): Option[UUci] = UUci.MMove.piotr(move)

  def readList(moves: String): Option[List[UUci]] =
    moves.split(' ').toList.map(apply(_)).sequence

  def writeList(moves: List[UUci]): String =
    moves.map(_.uci) mkString " "

  def readListPiotr(moves: String): Option[List[UUci]] =
    moves.split(' ').toList.map(piotr).sequence

  def writeListPiotr(moves: List[UUci]): String =
    moves.map(_.piotr) mkString " "
}