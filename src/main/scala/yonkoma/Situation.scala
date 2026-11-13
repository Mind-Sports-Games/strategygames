package strategygames.yonkoma

import strategygames.{ Player, Status }
import strategygames.yonkoma.format.Uci

import cats.data.Validated

case class Situation(board: Board, player: Player) {

  lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues(_.map(_.dest)).to(Map)

  // passing is not allowed, so the pass is only there for a player with no legal move,
  // whose turn is skipped
  lazy val canPass: Boolean = !end && moves.isEmpty

  lazy val forcedAction: Option[Action] = if (canPass) pass().toOption else None

  lazy val forcedTurnAction: Option[Action] = forcedAction

  def move(from: Pos, to: Pos): Validated[String, Move] =
    board.variant.move(this, from, to)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest)

  def pass(): Validated[String, Pass] = board.variant.pass(this)

  // a crosscut on the board; the moves above are the only move generation, done once
  lazy val variantEnd: Boolean = board.variant.specialEnd(this)

  lazy val end: Boolean = variantEnd

  lazy val winner: Option[Player] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean = (board valid strict) && !end

  lazy val status: Option[Status] =
    if (variantEnd) Some(Status.VariantEnd)
    else None

  def withVariant(variant: strategygames.yonkoma.variant.Variant) =
    copy(board = board withVariant variant)

  def unary_! = copy(player = !player)
}

object Situation {

  def apply(variant: strategygames.yonkoma.variant.Variant): Situation =
    Situation(Board init variant, variant.startPlayer)

}
