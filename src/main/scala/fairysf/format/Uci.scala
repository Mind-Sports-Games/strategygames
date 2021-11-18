package strategygames.fairysf.format
import strategygames.fairysf._

import strategygames.GameFamily

import cats.data.Validated
import cats.implicits._

sealed trait Uci {

  def uci: String
  def piotr: String

  def origDest: (Pos, Pos)

  def apply(situation: Situation): Validated[String, MoveOrDrop]
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

    def apply(situation: Situation) = situation.move(orig, dest, promotion) map Left.apply
  }

  object Move {

    def apply(gf: GameFamily, move: String): Option[Move] =
      for {
        orig <- Pos.fromKey(move take 2)
        dest <- Pos.fromKey(move.slice(2, 4))
        promotion = move.lift(4).flatMap(Role.promotable(gf, _))
      } yield Move(orig, dest, promotion)

    def piotr(gf: GameFamily, move: String) =
      for {
        orig <- move.headOption flatMap Pos.piotr
        dest <- move lift 1 flatMap Pos.piotr
        promotion = move.lift(2).flatMap(Role.promotable(gf, _))
      } yield Move(orig, dest, promotion)

    def fromStrings(gf: GameFamily, origS: String, destS: String, promS: Option[String]) =
      for {
        orig <- Pos.fromKey(origS)
        dest <- Pos.fromKey(destS)
        promotion = Role.promotable(gf, promS)
      } yield Move(orig, dest, promotion)

    val moveR = s"^${Pos.posR}${Pos.posR}(\\+?)$$".r

  }

  case class Drop(role: Role, pos: Pos) extends Uci {

    def uci = s"${role.pgn}@${pos.key}"

    def piotr = s"${role.pgn}@${pos.piotrStr}"

    def origDest = pos -> pos

    def apply(situation: Situation) = situation.drop(role, pos) map Right.apply
  }

  object Drop {

    def fromStrings(gf: GameFamily, roleS: String, posS: String) =
      for {
        role <- Role.allByName(gf) get roleS
        pos  <- Pos.fromKey(posS)
      } yield Drop(role, pos)

    val dropR = s"^${Role.roleR}@${Pos.posR}$$".r

  }

  case class WithSan(uci: Uci, san: String)

  def apply(move: strategygames.fairysf.Move) = Uci.Move(move.orig, move.dest, move.promotion)

  def apply(drop: strategygames.fairysf.Drop) = Uci.Drop(drop.piece.role, drop.pos)

  def apply(gf: GameFamily, move: String): Option[Uci] =
    if (move lift 1 contains '@') for {
      role <- move.headOption flatMap Role.allByPgn(gf).get
      pos  <- Pos.fromKey(move.slice(2, 4))
    } yield Uci.Drop(role, pos)
    else Uci.Move(gf, move)

  def piotr(gf: GameFamily, move: String): Option[Uci] =
    if (move lift 1 contains '@') for {
      role <- move.headOption flatMap Role.allByPgn(gf).get
      pos  <- move lift 2 flatMap Pos.piotr
    } yield Uci.Drop(role, pos)
    else Uci.Move.piotr(gf, move)

  def readList(gf: GameFamily, moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(apply(gf, _)).sequence

  def writeList(moves: List[Uci]): String =
    moves.map(_.uci) mkString " "

  def readListPiotr(gf: GameFamily, moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(piotr(gf, _)).sequence

  def writeListPiotr(moves: List[Uci]): String =
    moves.map(_.piotr) mkString " "
}
