package strategygames.dameo
package format

import cats.data.Validated
import cats.implicits._

sealed trait Uci {

  def uci: String
  def shortUci: String
  def piotr: String

  // def toSan: String
  // def toFullSan: String

  def origDest: Option[(Pos, Pos)]

  def apply(situation: Situation): Validated[String, Move]

}

object Uci {

  case class Move(
      orig: Pos,
      dest: Pos,
      promotion: Option[PromotableRole] = None,
      capture: Option[Pos] = None
  ) extends Uci {

    def keys     = orig.key + dest.key
    // TODO Dameo set these properly
    def uci      = orig.key + (if (capture.isEmpty) "" else "x") + dest.key
    def shortUci = orig.key + (if (capture.isEmpty) "" else "x") + dest.key

    def keysPiotr = orig.piotrStr + dest.piotrStr
    def piotr     = keysPiotr + promotionString

    def promotionString = promotion.fold("")(_.forsyth.toString)

    def origDest = Some(orig -> dest)

    def apply(situation: Situation) =
      situation.move(orig, dest, promotion, capture)

  }

  object Move {

    def apply(move: String): Option[Move] = {
      move match {
        case moveR(orig, capture, dest) =>
          (
            Pos.fromKey(orig),
            Pos.fromKey(dest),
            // TODO Dameo set this
            capture.length == 1
          ) match {
            case (Some(orig), Some(dest), capture) =>
              Some(
                Move(
                  orig = orig,
                  dest = dest,
                  // TODO Dameo set this
                  promotion = None,
                  capture = if (capture) Some(orig) else None
                )
              )
            case _                                 => None
          }
        case _                          => None
      }
    }

    // TODO Dameo use capture and promotion properly
    def piotr(move: String) = for {
      orig    <- move.headOption.flatMap(Pos.piotr)
      dest    <- move.lift(1).flatMap(Pos.piotr)
      capture <- move.lift(2).nonEmpty.some
    } yield Move(orig, dest, None, (if (capture) Some(orig) else None))

    // TODO Dameo use capture and promotion properly
    def fromStrings(origS: String, destS: String, promS: Option[String]) = for {
      orig     <- Pos.fromKey(origS)
      dest     <- Pos.fromKey(destS)
      promotion = Role promotable promS
    } yield Move(orig, dest, promotion, None)

    // TODO Dameo write the regex for each move. Do we need to add in promotion?
    val moveR = s"^${Pos.posR}([x]?)${Pos.posR}".r

  }

  case class WithSan(uci: Uci.Move, san: String)

  def apply(move: strategygames.dameo.Move, withCaptures: Boolean) =
    Uci.Move(move.orig, move.dest, move.promotion, if (withCaptures) move.capture else none)

  def combine(uci1: Uci.Move, uci2: Uci.Move) =
    apply(uci1.uci + uci2.uci.drop(2)).getOrElse(Uci.Move(uci1.orig, uci2.dest))
  def combineSan(san1: String, san2: String)  =
    san1.substring(0, san1.indexOf('x')) + san2.substring(san2.indexOf('x'))

  def apply(move: String): Option[Uci] = Uci.Move(move)

  def piotr(move: String): Option[Uci] = Uci.Move.piotr(move)

  def readList(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(apply).sequence

  def writeList(moves: List[Uci]): String =
    moves.map(_.uci) mkString " "

  def readListPiotr(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(piotr).sequence

  def writeListPiotr(moves: List[Uci]): String =
    moves.map(_.piotr) mkString " "

}
