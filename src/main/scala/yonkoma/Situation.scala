package strategygames.yonkoma

import strategygames.{ Player, Status }
import strategygames.yonkoma.format.Uci

import cats.data.Validated

case class Situation(board: Board, player: Player) {

  def history = board.history

  lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues(_.map(_.dest)).to(Map)

  // passing is not allowed, so the pass is only there for a player with no legal move,
  // whose turn is skipped
  lazy val canPass: Boolean = !end && moves.isEmpty

  lazy val canMove: Boolean = moves.nonEmpty

  def move(from: Pos, to: Pos): Validated[String, Move] =
    board.variant.move(this, from, to)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest)

  def pass(): Validated[String, Pass] = board.variant.pass(this)

  def variantEnd = board.variant.specialEnd(this)

  def end: Boolean = variantEnd

  def winner: Option[Player] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean = (board valid strict) && !end

  def opponentHasInsufficientMaterial: Boolean = false

  def autoDraw: Boolean = false

  lazy val status: Option[Status] =
    if (variantEnd) Some(Status.VariantEnd)
    else None

  def withHistory(history: History) =
    copy(board = board withHistory history)

  def withVariant(variant: strategygames.yonkoma.variant.Variant) =
    copy(board = board withVariant variant)

  def unary_! = copy(player = !player)
}

object Situation {

  def apply(variant: strategygames.yonkoma.variant.Variant): Situation =
    Situation(Board init variant, variant.startPlayer)

}
