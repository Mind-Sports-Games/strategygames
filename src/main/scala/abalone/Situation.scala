package strategygames.abalone

import strategygames.{ Player, Status }

import cats.data.Validated
import cats.implicits._

import strategygames.abalone.format.Uci

case class Situation(board: Board, player: Player) {

  def history = board.history

  def checkMate: Boolean = false

  def staleMate: Boolean = board.variant.specialDraw(this)

  def end: Boolean = staleMate || variantEnd

  def winner: Option[Player] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean =
    (board.valid(strict)) && !end

  // TODO Abalone set this
  // in case someone does not have more than 2 pieces, it could be considered as insufficient material to do anything
  // But I'm not sure we do not want to allow having a board containing only 1 marble for a player : for didactic consideration e.g. the famous "hunt as 4 v 1"
  def opponentHasInsufficientMaterial: Boolean =
    false

  def move(from: Pos, to: Pos, promotion: Option[PromotableRole]): Validated[String, Move] =
    board.variant.move(this, from, to, promotion)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest, uci.promotion)

  def withHistory(history: History) = copy(board = board.withHistory(history))

  def withVariant(variant: strategygames.abalone.variant.Variant) = copy(board = board.withVariant(variant))

  def unary_! = copy(player = !player)

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _.map(_.dest) }.to(Map)

  lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  lazy val status: Option[Status] =
    if (variantEnd) Status.VariantEnd.some
    else if (staleMate) Status.Stalemate.some
    else none

  private def variantEnd = board.variant.specialEnd(this)
}

object Situation {

  def apply(variant: strategygames.abalone.variant.Variant): Situation =
    Situation(Board init variant, variant.startPlayer)
}
