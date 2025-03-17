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
    val autoEndTurn: Boolean,
    val capture: Option[List[Pos]],
    val promotion: Option[PromotableRole] = None,
    val taken: Option[List[Pos]] = None,
    val castle: Option[((Pos, Pos), (Pos, Pos))] = None,
    val enpassant: Boolean = false,
    val metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore) {

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
  def toBackgammon: backgammon.Move
  def toAbalone: abalone.Move
  def toDameo: dameo.Move

}

object Move {

  final case class Chess(m: chess.Move)
      extends Move(
        Piece.Chess(m.piece),
        Pos.Chess(m.orig),
        Pos.Chess(m.dest),
        Situation.Chess(m.situationBefore),
        Board.Chess(m.after),
        m.autoEndTurn,
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
    def toGo           = sys.error("Can't make a go move from a chess move")
    def toBackgammon   = sys.error("Can't make a backgammon move from a chess move")
    def toAbalone      = sys.error("Can't make a abalone move from a chess move")
    def toDameo        = sys.error("Can't make a dameo move from a chess move")

  }

  final case class Draughts(m: draughts.Move)
      extends Move(
        Piece.Draughts(m.piece),
        Pos.Draughts(m.orig),
        Pos.Draughts(m.dest),
        Situation.Draughts(m.situationBefore),
        Board.Draughts(m.after),
        m.autoEndTurn,
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
    def toGo           = sys.error("Can't make a go move from a draughts move")
    def toBackgammon   = sys.error("Can't make a backgammon move from a draughts move")
    def toAbalone      = sys.error("Can't make a abalone move from a draughts move")
    def toDameo        = sys.error("Can't make a dameo move from a draughts move")

  }

  final case class FairySF(m: fairysf.Move)
      extends Move(
        Piece.FairySF(m.piece),
        Pos.FairySF(m.orig),
        Pos.FairySF(m.dest),
        Situation.FairySF(m.situationBefore),
        Board.FairySF(m.after),
        m.autoEndTurn,
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
    def toGo           = sys.error("Can't make a go move from a fairysf move")
    def toBackgammon   = sys.error("Can't make a backgammon move from a fairysf move")
    def toAbalone      = sys.error("Can't make a abalone move from a fairysf move")
    def toDameo        = sys.error("Can't make a dameo move from a fairysf move")

  }

  final case class Samurai(m: samurai.Move)
      extends Move(
        Piece.Samurai(m.piece),
        Pos.Samurai(m.orig),
        Pos.Samurai(m.dest),
        Situation.Samurai(m.situationBefore),
        Board.Samurai(m.after),
        m.autoEndTurn,
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
    def toGo           = sys.error("Can't make a go move from a samurai move")
    def toBackgammon   = sys.error("Can't make a backgammon move from a samurai move")
    def toAbalone      = sys.error("Can't make a abalone move from a samurai move")
    def toDameo        = sys.error("Can't make a dameo move from a samurai move")

  }

  final case class Togyzkumalak(m: togyzkumalak.Move)
      extends Move(
        Piece.Togyzkumalak(m.piece),
        Pos.Togyzkumalak(m.orig),
        Pos.Togyzkumalak(m.dest),
        Situation.Togyzkumalak(m.situationBefore),
        Board.Togyzkumalak(m.after),
        m.autoEndTurn,
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
    def toGo           = sys.error("Can't make a go move from a togyzkumalak move")
    def toBackgammon   = sys.error("Can't make a backgammon move from a togyzkumalak move")
    def toAbalone      = sys.error("Can't make a abalone move from a togyzkumalak move")
    def toDameo        = sys.error("Can't make a dameo move from a togyzkumalak move")

  }

  final case class Backgammon(m: backgammon.Move)
      extends Move(
        Piece.Backgammon(m.piece),
        Pos.Backgammon(m.orig),
        Pos.Backgammon(m.dest),
        Situation.Backgammon(m.situationBefore),
        Board.Backgammon(m.after),
        false,
        None,
        None,
        None,
        None,
        false,
        m.metrics
      ) {

    def situationAfter: Situation                          = Situation.Backgammon(m.situationAfter)
    def finalizeAfter(finalSquare: Boolean = false): Board = m.finalizeAfter

    def toUci: Uci.Move = Uci.BackgammonMove(m.toUci)

    def toShortUci: Uci.Move =
      Uci.Move(
        GameLogic.Backgammon(),
        orig,
        dest,
        promotion,
        if (capture.isDefined) capture.get.takeRight(1).some else None
      )

    def first: Move = this

    val unwrap         = m
    def toFairySF      = sys.error("Can't make a fairysf move from a backgammon move")
    def toChess        = sys.error("Can't make a chess move from a backgammon move")
    def toDraughts     = sys.error("Can't make a draughts move from a backgammon move")
    def toSamurai      = sys.error("Can't make a samurai move from a backgammon move")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak move from a backgammon move")
    def toGo           = sys.error("Can't make a go move from a backgammon move")
    def toBackgammon   = m
    def toAbalone      = sys.error("Can't make a abalone move from a backgammon move")
    def toDameo        = sys.error("Can't make a dameo move from a backgammon move")

  }

  final case class Abalone(m: abalone.Move)
      extends Move(
        m.situationBefore.board.getPiece(m.orig).get,
        Pos.Abalone(m.orig),
        Pos.Abalone(m.dest),
        Situation.Abalone(m.situationBefore),
        Board.Abalone(m.after),
        m.autoEndTurn,
        None, // capture. @TODO: check: we probably need to pass the infos here ?
        None,
        None,
        None,
        false,
        m.metrics
      ) {
    override def situationAfter: Situation                          = Situation.Abalone(m.situationAfter)
    override def finalizeAfter(finalSquare: Boolean = false): Board = m.finalizeAfter

    override def toUci: Uci.Move = Uci.AbaloneMove(m.toUci)

    override def toShortUci: Uci.Move =
      Uci.Move(
        GameLogic.Abalone(),
        orig,
        dest,
        promotion,
        if (capture.isDefined) capture.get.takeRight(1).some else None
      )

    override def first: Move = this

    val unwrap = m
    override def toFairySF      = sys.error("Can't make a fairysf move from a abalone move")
    override def toChess        = sys.error("Can't make a chess move from a abalone move")
    override def toDraughts     = sys.error("Can't make a draughts move from a abalone move")
    override def toSamurai      = sys.error("Can't make a samurai move from a abalone move")
    override def toTogyzkumalak = sys.error("Can't make a togyzkumalak move from a abalone move")
    override def toGo           = sys.error("Can't make a go move from a abalone move")
    override def toBackgammon   = sys.error("Can't make a backgammon move from a abalone move")
    override def toAbalone      = m
    override def toDameo        = sys.error("Can't make a dameo move from a abalone move")
  }

  final case class Dameo(m: dameo.Move)
      extends Move(
        Piece.Dameo(m.piece),
        Pos.Dameo(m.orig),
        Pos.Dameo(m.dest),
        Situation.Dameo(m.situationBefore),
        Board.Dameo(m.after),
        m.autoEndTurn,
        m.capture match {
          case Some(capture) => Option(List(Pos.Dameo(capture)))
          case None          => None
        },
        m.promotion match {
          case Some(promotion) => Some(Role.DameoPromotableRole(promotion))
          case None            => None
        },
        None,
        None,
        false,
        m.metrics
      ) {

    def situationAfter: Situation                          = Situation.Dameo(m.situationAfter)
    def finalizeAfter(finalSquare: Boolean = false): Board = m.finalizeAfter

    def toUci: Uci.Move = Uci.DameoMove(m.toUci)

    def toShortUci: Uci.Move =
      Uci.Move(
        GameLogic.Dameo(),
        orig,
        dest,
        promotion,
        if (capture.isDefined) capture.get.takeRight(1).some else None
      )

    def first: Move = this

    val unwrap         = m
    def toFairySF      = sys.error("Can't make a fairysf move from a dameo move")
    def toChess        = sys.error("Can't make a chess move from a dameo move")
    def toDraughts     = sys.error("Can't make a draughts move from a dameo move")
    def toSamurai      = sys.error("Can't make a samurai move from a dameo move")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak move from a dameo move")
    def toGo           = sys.error("Can't make a go move from a dameo move")
    def toBackgammon   = sys.error("Can't make a backgammon move from a dameo move")
    def toAbalone      = sys.error("Can't make a abalone move from a dameo move")
    def toDameo        = m

  }

  def wrap(m: chess.Move): Move        = Move.Chess(m)
  def wrap(m: draughts.Move): Move     = Move.Draughts(m)
  def wrap(m: fairysf.Move): Move      = Move.FairySF(m)
  def wrap(m: samurai.Move): Move      = Move.Samurai(m)
  def wrap(m: togyzkumalak.Move): Move = Move.Togyzkumalak(m)
  def wrap(m: backgammon.Move): Move   = Move.Backgammon(m)
  def wrap(m: abalone.Move): Move      = Move.Abalone(m)
  def wrap(m: dameo.Move): Move        = Move.Dameo(m)

}
