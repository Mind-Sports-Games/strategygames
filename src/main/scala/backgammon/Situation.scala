package strategygames.backgammon

import strategygames.{ Player, Status }

import cats.data.Validated
import cats.implicits._

import strategygames.backgammon.format.{ Forsyth, Uci }

case class Situation(board: Board, player: Player) {

  lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _ map (_.dest) }.to(Map)

  def history = board.history

  // TODO: Implement
  private lazy val gameEnd: Boolean = false

  // private lazy val gameResult: GameResult = board.apiPosition.gameResult

  // private lazy val result =
  //  if (gameEnd) gameResult
  //  else GameResult.Ongoing()

  // these dont exist in Oware. Normal ending tracked in VariantEnd
  def checkMate: Boolean = false
  def staleMate: Boolean = false

  private def variantEnd = false || board.variant.specialEnd(this)

  def end: Boolean = checkMate || staleMate || variantEnd

  def winner: Option[Player] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean =
    (board valid strict) && !end

  lazy val status: Option[Status] =
    if (checkMate) Status.Mate.some
    else if (variantEnd) Status.VariantEnd.some
    else if (staleMate) Status.Stalemate.some
    else none

  val oppTuzdik: Option[Pos] = board.pieces
    .filter {
      case (_, (p, _)) if p.role == Tuzdik && p.player != player => true
      case _                                                     => false
    }
    .map { case (pos, _) => pos }
    .headOption

  def opponentHasInsufficientMaterial: Boolean =
    if (player == P1) (board.history.score.p1 == 81) else (board.history.score.p2 == 81)

  def move(from: Pos, to: Pos, promotion: Option[PromotableRole]): Validated[String, Move] =
    board.variant.move(this, from, to, promotion)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest, uci.promotion)

  def withHistory(history: History) =
    copy(
      board = board withHistory history
    )

  def withVariant(variant: strategygames.backgammon.variant.Variant) =
    copy(
      board = board withVariant variant
    )

  def unary_! = copy(player = !player)
}

object Situation {

  def apply(variant: strategygames.backgammon.variant.Variant): Situation =
    Situation(Board init variant, variant.startPlayer)
}
