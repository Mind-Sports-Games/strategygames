package strategygames.chess

import strategygames.{ Player, Status }

import cats.data.Validated
import cats.implicits._

import strategygames.chess.format.Uci

case class Situation(board: Board, player: Player) {

  lazy val actors = board actorsOf player

  lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  lazy val playerCanCapture: Boolean = moves exists (_._2 exists (_.captures))

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _ map (_.dest) }.to(Map)

  def drops: Option[List[Pos]] =
    board.variant match {
      case v: variant.Crazyhouse.type => v possibleDrops this
      case _                          => None
    }

  def dropsByRole: Option[Map[Role, List[Pos]]] =
    board.variant match {
      case v: variant.Crazyhouse.type => v possibleDropsByRole this
      case _                          => None
    }

  lazy val kingPos: Option[Pos] = board kingPosOf player

  lazy val check: Boolean = board check player

  def checkSquare = if (check) kingPos else None

  def history = board.history

  def checkMate: Boolean = board.variant checkmate this

  def staleMate: Boolean = board.variant staleMate this

  def autoDraw: Boolean = board.autoDraw || board.variant.specialDraw(this)

  def opponentHasInsufficientMaterial: Boolean = board.variant.opponentHasInsufficientMaterial(this)

  lazy val threefoldRepetition: Boolean = board.history.threefoldRepetition

  def variantEnd = board.variant specialEnd this

  def end: Boolean = checkMate || staleMate || autoDraw || variantEnd

  def winner: Option[Player] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean =
    (board valid strict) && !end && !copy(player = !player).check

  lazy val status: Option[Status] =
    if (checkMate) Status.Mate.some
    else if (variantEnd) Status.VariantEnd.some
    else if (staleMate) Status.Stalemate.some
    else if (autoDraw) Status.Draw.some
    else none

  def move(from: Pos, to: Pos, promotion: Option[PromotableRole]): Validated[String, Move] =
    board.variant.move(this, from, to, promotion)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest, uci.promotion)

  def drop(role: Role, pos: Pos): Validated[String, Drop] =
    board.variant.drop(this, role, pos)

  def fixCastles = copy(board = board fixCastles)

  def withHistory(history: History) =
    copy(
      board = board withHistory history
    )

  def withVariant(variant: strategygames.chess.variant.Variant) =
    copy(
      board = board withVariant variant
    )

  def canCastle = board.history.canCastle _

  def enPassantSquare: Option[Pos] = {
    // Before potentially expensive move generation, first ensure some basic
    // conditions are met.
    // TODO Review for multiaction variants like Monster/Progressive
    history.lastAction match {
      case Some(move: Uci.Move) =>
        if (
          move.dest.yDist(move.orig) == 2 &&
          board(move.dest).exists(_.is(Pawn)) &&
          List(
            move.dest.file.offset(-1),
            move.dest.file.offset(1)
          ).flatten
            .flatMap(board(_, Rank.passablePawnRank(player)))
            .exists(_ == Piece(player, Pawn))
        )
          moves.values.flatten.find(_.enpassant).map(_.dest)
        else None
      case _                    => None
    }
  }

  def lastActionOfTurn: Boolean = board.variant.lastActionOfTurn(this)

  def unary_! = copy(player = !player)
}

object Situation {

  def apply(variant: strategygames.chess.variant.Variant): Situation =
    Situation(Board init variant, variant.startPlayer)
}
