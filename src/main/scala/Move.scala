package strategygames

import strategygames.MoveMetrics

import strategygames.chess.format.Uci
import cats.syntax.option._

sealed class Move(
  val piece: Piece,
  val orig: Pos,
  val dest: Pos,
  val situationBefore: Situation,
  val after: Board,
  val capture: Option[Pos],
  val promotion: Option[PromotableRole] = None,
  val taken: Option[List[Pos]] = None,
  val castle: Option[((Pos, Pos), (Pos, Pos))] = None,
  val enpassant: Boolean = false,
  val metrics: MoveMetrics = MoveMetrics()
) {

  def before = situationBefore.board

  def situationAfter: Situation

  def withHistory(h: History): Move

  def applyVariantEffect: Move = before.variant addVariantEffect this

  def captures: Boolean

  def promotes = promotion.isDefined

  def color = piece.color

  def withPromotion(op: Option[PromotableRole]): Option[Move]

  def withAfter(newBoard: Board): Move

  def withMetrics(m: MoveMetrics): Move

  def toUci: Uci.Move

  override def toString = s"$piece ${toUci.uci}"

}


final case class Chess(m: chess.Move) extends Move(
  Piece.Chess(m.piece),
  Pos.Chess(m.orig),
  Pos.Chess(m.dest),
  Situation.Chess(m.situationBefore),
  Board.Chess(m.after),
  m.capture match {
    case Some(capture) => Pos.Chess(capture).some
    case None          => None
  },
  m.promotion match {
    case Some(promotion) => PromotableRole.Chess(promotion).some
    case None            => None
  },
  None,
  m.castle match {
    case Some(((p1, p2), (p3, p4))) => ((Pos.Chess(p1), Pos.Chess(p2)), (Pos.Chess(p3), Pos.Chess(p4))).some
    case None                       => None
  },
  m.enpassant,
  m.metrics
){

  def situationAfter: Situation = m.situationAfter()

  def withHistory(h: History): Move = h match {
    case History.Chess(h) => Chess(m.withHistory(h))
    case _ => sys.error("Not passed Chess objects")
  }

  def captures: Boolean = m.captures

  def withPromotion(op: Option[PromotableRole]): Option[Move] = op match {
    case Some(PromotableRole.Chess(op)) => m.withPromotion(Some(op)).map(Chess)
    case None                           => m.withPromotion(None).map(Chess)
    case _ => sys.error("Not passed Chess objects")
  }

  def withAfter(newBoard: Board): Move = newBoard match {
    case Board.Chess(newBoard) => Chess(m.withAfter(newBoard))
    case _ => sys.error("Not passed Chess objects")
  }

  def withMetrics(m: MoveMetrics): Move = Chess(m.withMetrics(m))

  def toUci: Uci.Move = Uci.Move.Chess(m.toUci)

}

final case class Draughts(m: draughts.Move) extends Move(
  Piece.Draughts(m.piece),
  Pos.Draughts(m.orig),
  Pos.Draughts(m.dest),
  Situation.Draughts(m.situationBefore),
  Board.Draughts(m.after),
  m.capture match {
    case Some(capture) => capture.map(Pos.Draughts(_)).toList().some
    case None          => None
  },
  m.promotion match {
    case Some(promotion) => PromotableRole.Draughts(promotion).some
    case None            => None
  },
  m.taken match {
    case Some(taken) => taken.map(Pos.Draughts(_)).toList().some
    case None        => None
  },
  None,
  false,
  m.metrics
){

  def situationAfter: Situation = m.situationAfter()

  def withHistory(h: History): Move = h match {
    case History.Draughts(h) => Draughts(m.withHistory(h))
    case _ => sys.error("Not passed Draughts objects")
  }

  def captures: Boolean = m.captures

  def withPromotion(op: Option[PromotableRole]): Option[Move] = op match {
    case Some(PromotableRole.Draughts(op)) => m.withPromotion(Some(op)).map(Draughts)
    case None                           => m.withPromotion(None).map(Draughts)
    case _ => sys.error("Not passed Draughts objects")
  }

  def withAfter(newBoard: Board): Move = newBoard match {
    case Board.Draughts(newBoard) => Draughts(m.withAfter(newBoard))
    case _ => sys.error("Not passed Draughts objects")
  }

  def withMetrics(m: MoveMetrics): Move = Draughts(m.withMetrics(m))

  def toUci: Uci.Move = Uci.Move.Draughts(m.toUci)

}
