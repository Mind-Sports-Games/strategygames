package strategygames.mancala

import strategygames.{ Player, Status }

import cats.data.Validated
import cats.implicits._

import strategygames.mancala.format.{ Forsyth, Uci }

case class Situation(board: Board, player: Player) {

  lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _ map (_.dest) }.to(Map)

  def history = board.history

  private lazy val gameEnd: Boolean = false

  private lazy val gameResult: GameResult = GameResult.Ongoing()

  private lazy val result =
    if (gameEnd) gameResult
    else GameResult.Ongoing()

  def checkMate: Boolean = result == GameResult.Checkmate()

  def staleMate: Boolean = result == GameResult.Stalemate()

  private def variantEnd = result == GameResult.VariantEnd() || board.variant.specialEnd(this)

  def end: Boolean = checkMate || staleMate || variantEnd

  def winner: Option[Player] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean =
    (board valid strict) && !end

  lazy val status: Option[Status] =
    if (checkMate) Status.Mate.some
    else if (variantEnd) Status.VariantEnd.some
    else if (staleMate) Status.Stalemate.some
    else none

  //TODO: test P1/P2 map is correct
  def opponentHasInsufficientMaterial: Boolean = false

  def move(from: Pos, to: Pos, promotion: Option[PromotableRole]): Validated[String, Move] =
    board.variant.move(this, from, to, promotion)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest, uci.promotion)

  def withVariant(variant: strategygames.mancala.variant.Variant) =
    copy(
      board = board withVariant variant
    )

  def unary_! = copy(player = !player)
}

object Situation {

  def apply(variant: strategygames.mancala.variant.Variant): Situation =
    Situation(Board init variant, variant.startPlayer)
}
