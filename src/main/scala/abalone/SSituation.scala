package abalone

import abalone.format.UUci
import abalone.util.geometry.Cell
import cats.data.Validated
import cats.implicits._
import strategygames.abalone.variant.Variant
import strategygames.{Player, Status}

case class SSituation(board: BBoard, player: Player) {
  def stalemate: Boolean = board.variant.specialDraw(this)

  def autoDraw: Boolean = board.autoDraw

  def end: Boolean = stalemate || autoDraw || variantEnd

  def winner: Option[Player] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean = (board.valid(strict)) && !end

  def opponentHasInsufficientMaterial: Boolean = false

  def move(from: Cell, to: Cell): Validated[String, MMove] =
    board.variant.move(this, from, to)

  def move(uci: UUci.MMove): Validated[String, MMove] =
    board.variant.move(this, uci.orig, uci.dest)

  def withHistory(history: HHistory) = copy(board = board.withHistory(history))

  def withVariant(variant: Variant) = copy(board = board.withVariant(variant))

  def unary_! = copy(player = !player)

  lazy val destinations: Map[Cell, List[Cell]] = moves.view.mapValues {
    _.map(_.dest)
  }.to(Map)

  lazy val moves: Map[Cell, List[MMove]] = board.variant.validMoves(this)

  lazy val status: Option[Status] =
    if (variantEnd) Status.VariantEnd.some
    else if (stalemate) Status.Stalemate.some
    else if (autoDraw) Status.Draw.some
    else none

  private def variantEnd = board.variant.specialEnd(this)
}

object SSituation {
  def apply(variant: Variant): SSituation =
    SSituation(BBoard init variant, variant.startPlayer)
}