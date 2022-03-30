package strategygames.mancala.format
import strategygames.mancala._

import strategygames.GameFamily

import cats.data.Validated
import cats.implicits._

sealed trait Uci {

  def uci: String
  def piotr: String

  def origDest: (Pos, Pos)

  def apply(situation: Situation): Validated[String, Move]
}

object Uci {

  case class Move(
      orig: Pos,
      dest: Pos,
      promotion: Option[PromotableRole] = None
  ) extends Uci {

    def keys = orig.key + dest.key
    def uci  = keys + promotionString

    def keysPiotr = orig.piotrStr + dest.piotrStr
    def piotr     = keysPiotr + promotionString

    def promotionString = promotion.fold("")(_.forsyth.toString)

    def origDest = orig -> dest

    def apply(situation: Situation) = situation.move(orig, dest, promotion)
  }

  object Move {

    def apply(move: String): Option[Move] =
      move match { 
        case moveP(orig, dest, promotion) => (
          Pos.fromKey(orig),
          Pos.fromKey(dest),
          promotion
        ) match {
          case (Some(orig), Some(dest), _) => {
            Move(
              orig = orig,
              dest = dest,
              promotion = None
            ).some
          }
          case _ => None
        }
        case _ => None
      }

    def piotr(move: String) =
      for {
        orig <- move.headOption flatMap Pos.piotr
        dest <- move lift 1 flatMap Pos.piotr
      } yield Move(orig, dest, promotion=None)

    def fromStrings(gf: GameFamily, origS: String, destS: String, promS: Option[String]) =
      for {
        orig <- Pos.fromKey(origS)
        dest <- Pos.fromKey(destS)
        promotion = None
      } yield Move(orig, dest, promotion)

    val moveR = s"^${Pos.posR}${Pos.posR}(\\+?)$$".r
    val moveP = s"^${Pos.posR}${Pos.posR}${Role.roleRr}$$".r
    val movePR = s"^${Pos.posR}${Pos.posR}${Role.rolePR}$$".r
  }

  case class WithSan(uci: Uci, san: String)

  def apply(move: strategygames.mancala.Move) = Uci.Move(move.orig, move.dest, move.promotion)

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
