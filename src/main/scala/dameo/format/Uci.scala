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
      promotion: Option[PromotableRole] = None
  ) extends Uci {
    /* Uci encodes the start and end position of a move and whether a promotion happened.
    Whether this results in capture must be computed separately with reference to the board state.
     */

    def keys            = orig.key + dest.key
    def promotionString = promotion.fold("")(_.forsyth.toString)
    def uci             = orig.key + dest.key + promotionString
    def shortUci        = uci

    def keysPiotr = orig.piotrStr + dest.piotrStr
    def piotr     = keysPiotr + promotion.fold("")(r => "p" + r.forsyth)

    def origDest = Some(orig -> dest)

    def apply(situation: Situation) =
      situation.move(orig, dest, promotion)

  }

  object Move {

    def apply(move: String): Option[Move] = {
      move match {
        case moveR(orig, dest, promotion) =>
          (
            Pos.fromKey(orig),
            Pos.fromKey(dest),
            promotion == "K"
          ) match {
            case (Some(orig), Some(dest), promotion) =>
              Some(
                Move(
                  orig = orig,
                  dest = dest,
                  promotion = if (promotion) Some(King) else None
                )
              )
            case _                                   => None
          }
        case _                            => None
      }
    }

    def piotr(move: String): Option[Move] = {
      move match {
        case piotrR(piotrOrig, piotrDest, promotion) =>
          (
            piotrOrig.headOption.flatMap(Pos.piotr),
            piotrDest.headOption.flatMap(Pos.piotr),
            promotion
          ) match {
            case (Some(orig), Some(dest), promotion) =>
              Some(
                Move(
                  orig,
                  dest,
                  promotion = if (promotion == "") None else Some(King)
                )
              )
            case _                                   => None
          }
        case _                                       => None
      }
    }

    def fromStrings(origS: String, destS: String, promS: Option[String]) = for {
      orig     <- Pos.fromKey(origS)
      dest     <- Pos.fromKey(destS)
      promotion = Role promotable promS
    } yield Move(orig, dest, promotion)

    val moveR  = s"^${Pos.posR}${Pos.posR}([K]?)".r
    val piotrR = "(.)(.)((?:pK)?)".r
  }

  case class WithSan(uci: Uci.Move, san: String)

  def apply(move: strategygames.dameo.Move) =
    Uci.Move(move.orig, move.dest, move.promotion)

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
