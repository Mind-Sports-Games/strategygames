package strategygames.abalone

import strategygames.{ Player, Status }

import cats.data.Validated
import cats.implicits._

import strategygames.abalone.format.Uci

case class Situation(board: Board, player: Player) {

  def history = board.history

  def checkMate: Boolean = false

  def staleMate: Boolean = board.variant.specialDraw(this)

  def autoDraw: Boolean = board.autoDraw

  def end: Boolean = staleMate || autoDraw || variantEnd

  def winner: Option[Player] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean = (board.valid(strict)) && !end

  def opponentHasInsufficientMaterial: Boolean = false

  def move(from: Pos, to: Pos): Validated[String, Move] =
    board.variant.move(this, from, to)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest)

  def withHistory(history: History) = copy(board = board.withHistory(history))

  def withVariant(variant: strategygames.abalone.variant.Variant) = copy(board = board.withVariant(variant))

  def unary_! = copy(player = !player)

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _.map(_.dest) }.to(Map)

  lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  lazy val status: Option[Status] =
    if (variantEnd) Status.VariantEnd.some
    else if (staleMate) Status.Stalemate.some
    else if (autoDraw) Status.Draw.some
    else none

  private def variantEnd = board.variant.specialEnd(this)
}

object Situation {

  def apply(variant: strategygames.abalone.variant.Variant): Situation =
    Situation(Board init variant, variant.startPlayer)
}
