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
  def finalizeAfter(finalSquare: Boolean = false): Board

  def promotes = promotion.isDefined

  def color = piece.color

  def toUci: Uci.Move
  //only used by draughts but making available for all
  def toShortUci: Uci.Move

  override def toString = s"$piece ${toUci.uci}"

  // TODO: Yup, still not type safe. :D
  def toChess: chess.Move
  def toDraughts: draughts.Move
  def toFairySF: fairysf.Move

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
    def finalizeAfter(_finalSquare: Boolean = false): Board = m.finalizeAfter

    def toUci: Uci.Move = Uci.ChessMove(m.toUci)

    //only used by draughts
    def toShortUci: Uci.Move =
      Uci.Move(
        GameLogic.Chess(),
        orig,
        dest,
        promotion,
        if (capture.isDefined) capture.get.takeRight(1).some else None
      )

    val unwrap = m
    def toChess = m
    def toDraughts = sys.error("Can't make a draughts move from a chess move")
    def toFairySF = sys.error("Can't make a fairysf move from a chess move")

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
    def finalizeAfter(finalSquare: Boolean = false): Board = m.finalizeAfter(finalSquare)

    def toUci: Uci.Move = Uci.DraughtsMove(m.toUci)

    def toShortUci: Uci.Move =
      Uci.Move(
        GameLogic.Draughts(),
        orig,
        dest,
        promotion,
        if (capture.isDefined) capture.get.takeRight(1).some else None
      )

    val unwrap = m
    def toDraughts = m
    def toChess = sys.error("Can't make a chess move from a draughts move")
    def toFairySF = sys.error("Can't make a fairysf move from a draughts move")

  }

  final case class FairySF(m: fairysf.Move) extends Move(
    Piece.FairySF(m.piece),
    Pos.FairySF(m.orig),
    Pos.FairySF(m.dest),
    Situation.FairySF(m.situationBefore),
    Board.FairySF(m.after),
    m.capture match {
      case Some(capture) => Option(List(Pos.FairySF(capture)))
      case None          => None
    },
    m.promotion match {
      case Some(promotion) => Option(Role.FairySFPromotableRole(promotion))
      case None            => None
    },
    None,
    m.castle match {
      case Some(((p1, p2), (p3, p4))) => ((Pos.FairySF(p1), Pos.FairySF(p2)), (Pos.FairySF(p3), Pos.FairySF(p4))).some
      case None                       => None
    },
    m.enpassant,
    m.metrics
  ){

    def situationAfter: Situation = Situation.FairySF(m.situationAfter)
    def finalizeAfter(finalSquare: Boolean = false): Board = m.finalizeAfter

    def toUci: Uci.Move = Uci.FairySFMove(m.toUci)

    def toShortUci: Uci.Move =
      Uci.Move(
        GameLogic.FairySF(),
        orig,
        dest,
        promotion,
        if (capture.isDefined) capture.get.takeRight(1).some else None
      )

    val unwrap = m
    def toFairySF = m
    def toChess = sys.error("Can't make a chess move from a fairysf move")
    def toDraughts = sys.error("Can't make a draughts move from a fairysf move")

  }

  def wrap(m: chess.Move): Move = Move.Chess(m)
  def wrap(m: draughts.Move): Move = Move.Draughts(m)
  def wrap(m: fairysf.Move): Move = Move.FairySF(m)

  def toChess(moveOrDrop: MoveOrDrop): chess.MoveOrDrop = moveOrDrop.left.map(_.toChess).right.map(_.toChess)
  //probably not type safe
  def toDraughts(moveOrDrop: MoveOrDrop): draughts.Move = moveOrDrop.left.map(_.toDraughts).left.get
  def toFairySF(moveOrDrop: MoveOrDrop): fairysf.MoveOrDrop = moveOrDrop.left.map(_.toFairySF).right.map(_.toFairySF)

}
