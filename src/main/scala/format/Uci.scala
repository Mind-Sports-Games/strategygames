package strategygames.format

import strategygames._

import cats.data.Validated
import cats.implicits._

sealed trait Uci {

  def uci: String
  def piotr: String

  def origDest: (Pos, Pos)

}

object Uci {

  final case class Chess(u: chess.format.Uci) extends Uci
  final case class Draughts(u: draughts.format.Uci) extends Uci

  sealed class Move(
      orig: Pos,
      dest: Pos,
      promotion: Option[PromotableRole] = None,
      capture: Option[List[Pos]] = None
  ) extends Uci {

    def keys = orig.key + dest.key
    def uci: String

    def keysPiotr = orig.piotrStr + dest.piotrStr
    def piotr     = keysPiotr + promotionString

    def promotionString = promotion.fold("")(_.forsyth.toString)

    def origDest = orig -> dest

  }

  final case class ChessMove(m: chess.format.Uci.Move) extends Move(
    Pos.Chess(m.orig),
    Pos.Chess(m.dest),
    m.promotion.map(ChessPromotableRole)
  ){
    def uci = m.uci
  }

  final case class DraughtsMove(m: draughts.format.Uci.Move) extends Move(
    Pos.Draughts(m.orig),
    Pos.Draughts(m.dest),
    m.promotion.map(DraughtsPromotableRole),
    m.capture.map(_.flatMap(p => p match {
      case Pos.Draughts(p) => Some(p)
      case _               => None
    }))
  ){
    def uci = m.uci
  }

  object Move {

    def apply(lib: GameLib, move: String): Option[Move] = lib match {
      case GameLib.Draughts() => draughts.format.Uci.Move.apply(move).map(DraughtsMove)
      case GameLib.Chess()    => chess.format.Uci.Move.apply(move).map(ChessMove)
    }

    def piotr(lib: GameLib, move: String): Option[Move] = lib match {
      case GameLib.Draughts() => draughts.format.Uci.Move.piotr(move).map(DraughtsMove)
      case GameLib.Chess()    => chess.format.Uci.Move.piotr(move).map(ChessMove)
    }

    def fromStrings(lib: GameLib, origS: String, destS: String, promS: Option[String]): Option[Move] = lib match {
      case GameLib.Draughts() => draughts.format.Uci.Move.fromStrings(origS, destS, promS).map(DraughtsMove)
      case GameLib.Chess()    => chess.format.Uci.Move.fromStrings(origS, destS, promS).map(ChessMove)
    }
  }

  case class WithSan(uci: Uci, san: String)

  def apply(lib: GameLib, move: String): Option[Uci] = lib match {
      case GameLib.Draughts() => draughts.format.Uci.apply(move).map(Draughts)
      case GameLib.Chess()    => chess.format.Uci.apply(move).map(Chess)
  }

  def piotr(lib: GameLib, move: String): Option[Uci] = lib match {
      case GameLib.Draughts() => draughts.format.Uci.piotr(move).map(Draughts)
      case GameLib.Chess()    => chess.format.Uci.piotr(move).map(Chess)
  }

  def readList(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(apply).sequence

  def writeList(moves: List[Uci]): String =
    moves.map(_.uci) mkString " "

  def readListPiotr(moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(piotr).sequence

  def writeListPiotr(moves: List[Uci]): String =
    moves.map(_.piotr) mkString " "
}
