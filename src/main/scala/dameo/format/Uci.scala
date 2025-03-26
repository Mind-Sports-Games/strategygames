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
    /* A man only promotes when he *ends* his turn on the back rank, however since
    Dameo uses multi-action turns there could be partial turns that end on the back
    rank, yet don't cause a promotion, because they don't end the full turn. So it is
    probably easiest to encode promotion explicitly here.
    Also, we encode the position of the captured stone explicitly, to make it easy to
    get back the captured stone from a long leap capture by a king.
    */

    def keys     = orig.key + dest.key
    def promotionString = promotion.fold("")(_.forsyth.toString)
    def uci      = orig.key + capture.fold("")(c => "x" + c.key) + dest.key + promotionString
    def shortUci = uci

    def keysPiotr = orig.piotrStr + dest.piotrStr
    def piotr     = keysPiotr + capture.fold("")(c => "x" + c.piotr) + promotion.fold("")(r => "p" + r.forsyth)


    def origDest = Some(orig -> dest)

    def apply(situation: Situation) =
      situation.move(orig, dest, promotion, capture)

  }

  object Move {

    def apply(move: String): Option[Move] = {
      move match {
        case moveR(orig, capture, dest, promotion) =>
          (
            Pos.fromKey(orig),
            Pos.fromKey(dest),
            capture,
            promotion == "k"
          ) match {
            case (Some(orig), Some(dest), capture, promotion) =>
              Some(
                Move(
                  orig = orig,
                  dest = dest,
                  promotion = if (promotion) Some(King) else None,
                  capture = if (capture == "") None else Pos.fromKey(capture.tail)
                )
              )
            case _ => None
          }
        case _ => None
      }
    }

    val piotrR = "(.)(.)((?:x.)?)((?:pk)?)".r

    def piotr(move: String): Option[Move] = {
      move match {
        case piotrR(orig, dest, capture, promotion) =>
          Some(Move(
            Pos.piotr(orig.head).get,
            Pos.piotr(dest.head).get,
            capture = if (capture == "") None else Pos.piotr(capture(1)),
            promotion = if (promotion == "") None else Some(King)
          ))
        case _ => None
      }
    }

    // TODO Dameo use capture and promotion properly
    def fromStrings(origS: String, destS: String, promS: Option[String]) = for {
      orig     <- Pos.fromKey(origS)
      dest     <- Pos.fromKey(destS)
      promotion = Role promotable promS
    } yield Move(orig, dest, promotion, None)

    val moveR = s"^(${Pos.posR})((?:x${Pos.posR})?)(${Pos.posR})([k]?)".r

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
