package strategygames

import strategygames.MoveMetrics

import strategygames.format.Uci
import cats.syntax.option._

sealed abstract class Move(
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

  def player = piece.player

  // Used to peel off the first part of a multi-move.
  // Must be implemented by the specializations if they
  // support multi moves
  def first: Move

  def toUci: Uci.Move
  // only used by draughts but making available for all
  def toShortUci: Uci.Move

  override def toString = s"$piece ${toUci.uci}"

  // TODO: Yup, still not type safe. :D
  def toChess: chess.Move
  def toDraughts: draughts.Move
  def toFairySF: fairysf.Move
  def toSamurai: samurai.Move
  def toTogyzkumalak: togyzkumalak.Move

}

object Move {

  final case class Chess(m: chess.Move)
      extends Move(
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
          case Some(((p1, p2), (p3, p4))) =>
            ((Pos.Chess(p1), Pos.Chess(p2)), (Pos.Chess(p3), Pos.Chess(p4))).some
          case None                       => None
        },
        m.enpassant,
        m.metrics
      ) {

    def situationAfter: Situation                           = Situation.Chess(m.situationAfter)
    def finalizeAfter(_finalSquare: Boolean = false): Board = m.finalizeAfter

    def toUci: Uci.Move = Uci.ChessMove(m.toUci)

    // only used by draughts
    def toShortUci: Uci.Move =
      Uci.Move(
        GameLogic.Chess(),
        orig,
        dest,
        promotion,
        if (capture.isDefined) capture.get.takeRight(1).some else None
      )

    def first: Move = this

    val unwrap         = m
    def toChess        = m
    def toDraughts     = sys.error("Can't make a draughts move from a chess move")
    def toFairySF      = sys.error("Can't make a fairysf move from a chess move")
    def toSamurai      = sys.error("Can't make a samurai move from a chess move")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak move from a chess move")

  }

  final case class Draughts(m: draughts.Move)
      extends Move(
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
      ) {

    def situationAfter: Situation                          = Situation.Draughts(m.situationAfter)
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

    def first: Move = copy(m = m.first)

    val unwrap         = m
    def toDraughts     = m
    def toChess        = sys.error("Can't make a chess move from a draughts move")
    def toFairySF      = sys.error("Can't make a fairysf move from a draughts move")
    def toSamurai      = sys.error("Can't make a samurai move from a draughts move")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak move from a draughts move")

  }

  final case class FairySF(m: fairysf.Move)
      extends Move(
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
          case Some(((p1, p2), (p3, p4))) =>
            ((Pos.FairySF(p1), Pos.FairySF(p2)), (Pos.FairySF(p3), Pos.FairySF(p4))).some
          case None                       => None
        },
        m.enpassant,
        m.metrics
      ) {

    def situationAfter: Situation                          = Situation.FairySF(m.situationAfter)
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

    def first: Move = this

    val unwrap         = m
    def toFairySF      = m
    def toChess        = sys.error("Can't make a chess move from a fairysf move")
    def toDraughts     = sys.error("Can't make a draughts move from a fairysf move")
    def toSamurai      = sys.error("Can't make a samurai move from a fairysf move")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak move from a fairysf move")

  }

  final case class Samurai(m: samurai.Move)
      extends Move(
        Piece.Samurai(m.piece),
        Pos.Samurai(m.orig),
        Pos.Samurai(m.dest),
        Situation.Samurai(m.situationBefore),
        Board.Samurai(m.after),
        m.capture match {
          case Some(capture) => Option(List(Pos.Samurai(capture)))
          case None          => None
        },
        None,
        None,
        None,
        false,
        m.metrics
      ) {

    def situationAfter: Situation                          = Situation.Samurai(m.situationAfter)
    def finalizeAfter(finalSquare: Boolean = false): Board = m.finalizeAfter

    def toUci: Uci.Move = Uci.SamuraiMove(m.toUci)

    def toShortUci: Uci.Move =
      Uci.Move(
        GameLogic.Samurai(),
        orig,
        dest,
        promotion,
        if (capture.isDefined) capture.get.takeRight(1).some else None
      )

    def first: Move = this

    val unwrap         = m
    def toFairySF      = sys.error("Can't make a fairysf move from a samurai move")
    def toChess        = sys.error("Can't make a chess move from a samurai move")
    def toDraughts     = sys.error("Can't make a draughts move from a samurai move")
    def toSamurai      = m
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak move from a samurai move")

  }

  final case class Togyzkumalak(m: togyzkumalak.Move)
      extends Move(
        Piece.Togyzkumalak(m.piece),
        Pos.Togyzkumalak(m.orig),
        Pos.Togyzkumalak(m.dest),
        Situation.Togyzkumalak(m.situationBefore),
        Board.Togyzkumalak(m.after),
        m.capture match {
          case Some(capture) => Option(List(Pos.Togyzkumalak(capture)))
          case None          => None
        },
        None,
        None,
        None,
        false,
        m.metrics
      ) {

    def situationAfter: Situation                          = Situation.Togyzkumalak(m.situationAfter)
    def finalizeAfter(finalSquare: Boolean = false): Board = m.finalizeAfter

    def toUci: Uci.Move = Uci.TogyzkumalakMove(m.toUci)

    def toShortUci: Uci.Move =
      Uci.Move(
        GameLogic.Togyzkumalak(),
        orig,
        dest,
        promotion,
        if (capture.isDefined) capture.get.takeRight(1).some else None
      )

    def first: Move = this

    val unwrap         = m
    def toFairySF      = sys.error("Can't make a fairysf move from a togyzkumalak move")
    def toChess        = sys.error("Can't make a chess move from a togyzkumalak move")
    def toDraughts     = sys.error("Can't make a draughts move from a togyzkumalak move")
    def toSamurai      = sys.error("Can't make a samurai move from a togyzkumalak move")
    def toTogyzkumalak = m

  }

  def wrap(m: chess.Move): Move        = Move.Chess(m)
  def wrap(m: draughts.Move): Move     = Move.Draughts(m)
  def wrap(m: fairysf.Move): Move      = Move.FairySF(m)
  def wrap(m: samurai.Move): Move      = Move.Samurai(m)
  def wrap(m: togyzkumalak.Move): Move = Move.Togyzkumalak(m)

  def toChess(moveOrDrop: MoveOrDrop): chess.MoveOrDrop         = moveOrDrop.left.map(_.toChess).right.map(_.toChess)
  // probably not type safe
  def toDraughts(moveOrDrop: MoveOrDrop): draughts.Move         = moveOrDrop.left.map(_.toDraughts).left.get
  def toFairySF(moveOrDrop: MoveOrDrop): fairysf.MoveOrDrop     =
    moveOrDrop.left.map(_.toFairySF).right.map(_.toFairySF)
  def toSamurai(moveOrDrop: MoveOrDrop): samurai.Move           = moveOrDrop.left.map(_.toSamurai).left.get
  def toTogyzkumalak(moveOrDrop: MoveOrDrop): togyzkumalak.Move =
    moveOrDrop.left.map(_.toTogyzkumalak).left.get

}
