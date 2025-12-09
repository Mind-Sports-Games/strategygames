package strategygames.dameo

import strategygames.{ Player, Status }
import strategygames.dameo.format.Uci

import cats.data.Validated
//import cats implicits in this file causes mindtrap to have problems
//import cats.implicits._

case class Situation(board: Board, player: Player) {

  lazy val actors = board.actorsOf(player)

  lazy val ghosts = board.ghosts

  lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _ map (_.dest) }.to(Map)

  def history = board.history

  def autoDraw: Boolean = board.autoDraw

  def variantEnd = board.variant.variantEnd(this)

  def end: Boolean = variantEnd || autoDraw

  def winner: Option[Player] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean =
    (board valid strict) && !end

  def opponentHasInsufficientMaterial: Boolean = false

  lazy val status: Option[Status] =
    if (variantEnd) Some(Status.VariantEnd)
    else if (autoDraw) Some(Status.Draw)
    else None

  lazy val allMovesCaptureLength: Int =
    actors.foldLeft(0) { case (max, actor) =>
      Math.max(actor.captureLength, max)
    }

  def captureLengthFrom(pos: Pos): Option[Int] =
    actorAt(pos).map(_.captureLength)

  def actorAt(pos: Pos): Option[Actor] = board.actorAt(pos)

  def move(
      from: Pos,
      to: Pos,
      promotion: Option[PromotableRole] = None
  ): Validated[String, Move] =
    board.variant.move(this, from, to, promotion)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest, uci.promotion)

  def withHistory(history: History) =
    copy(
      board = board withHistory history
    )

  def withVariant(variant: strategygames.dameo.variant.Variant) =
    copy(
      board = board withVariant variant
    )

  def unary_! = copy(player = !player)
}

object Situation {

  def apply(variant: strategygames.dameo.variant.Variant): Situation =
    Situation(Board init variant, variant.startPlayer)

}
