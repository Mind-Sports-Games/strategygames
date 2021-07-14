package strategygames

import strategygames.MoveMetrics

import strategygames.format.Uci
import cats.syntax.option._

abstract sealed class Move(
  val piece: Piece,
  val orig: Pos,
  val dest: Pos,
  val situationBefore: Situation,
  val after: Board,
  val capture: Option[List[Pos]],
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

object Move {

  final case class Chess(m: chess.Move) extends Move(
    Piece.Chess(m.piece),
    Pos.Chess(m.orig),
    Pos.Chess(m.dest),
    Situation.Chess(m.situationBefore),
    Board.Chess(m.after),
    m.capture match {
      case Some(capture) => Option(List(Pos.Chess(capture)))
      case None          => None
    },
    m.promotion match {
      case Some(promotion) => Option(Role.ChessPromotableRole(promotion))
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

    def situationAfter: Situation = Situation.Chess(m.situationAfter)

    def withHistory(h: History): Move = h match {
      case History.Chess(h) => Chess(m.withHistory(h))
      case _ => sys.error("Not passed Chess objects")
    }

    def captures: Boolean = m.captures

    def withPromotion(op: Option[PromotableRole]): Option[Move] = op match {
      case Some(Role.ChessPromotableRole(op)) => m.withPromotion(Some(op)).map(Chess)
      case None                               => m.withPromotion(None).map(Chess)
      case _ => sys.error("Not passed Chess objects")
    }

    def withAfter(newBoard: Board): Move = newBoard match {
      case Board.Chess(newBoard) => Chess(m.withAfter(newBoard))
      case _ => sys.error("Not passed Chess objects")
    }

    def withMetrics(mm: MoveMetrics): Move = Move.Chess(m.withMetrics(mm))

    def toUci: Uci.Move = Uci.ChessMove(m.toUci)

  }

  final case class Draughts(m: draughts.Move) extends Move(
    Piece.Draughts(m.piece),
    Pos.Draughts(m.orig),
    Pos.Draughts(m.dest),
    Situation.Draughts(m.situationBefore),
    Board.Draughts(m.after),
    m.capture match {
      case Some(capture) => Some(capture.map(Pos.Draughts))
      case None          => None
    },
    m.promotion match {
      case Some(promotion) => Some(Role.DraughtsPromotableRole(promotion))
      case None            => None
    },
    m.taken match {
      case Some(taken) => Some(taken.map(Pos.Draughts))
      case None        => None
    },
    None,
    false,
    m.metrics
  ){

    def situationAfter: Situation = Situation.Draughts(m.situationAfter)

    def withHistory(h: History): Move = h match {
      case History.Draughts(h) => Draughts(m.withHistory(h))
      case _ => sys.error("Not passed Draughts objects")
    }

    def captures: Boolean = m.captures

    def withPromotion(op: Option[PromotableRole]): Option[Move] = op match {
      case Some(Role.DraughtsPromotableRole(op)) => m.withPromotion(Some(op)).map(Draughts)
      case None                                  => m.withPromotion(None).map(Draughts)
      case _ => sys.error("Not passed Draughts objects")
    }

    def withAfter(newBoard: Board): Move = newBoard match {
      case Board.Draughts(newBoard) => Draughts(m.withAfter(newBoard))
      case _ => sys.error("Not passed Draughts objects")
    }

    def withMetrics(mm: MoveMetrics): Move = Move.Draughts(m.withMetrics(mm))

    def toUci: Uci.Move = Uci.DraughtsMove(m.toUci)

  }

}
