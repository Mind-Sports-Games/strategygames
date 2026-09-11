package strategygames.entropy

import strategygames.{ Player, Status }
import strategygames.entropy.format.Uci

import cats.data.Validated

case class Situation(board: Board, player: Player) {

  def history = board.history

  def chaosPlayer: Player = board.chaosPlayer
  def orderPlayer: Player = board.orderPlayer

  def isChaos: Boolean = player == chaosPlayer
  def isOrder: Boolean = player == orderPlayer

  // Chaos must reveal a counter before it has anything to place. Read from the pocket
  // rather than the turn history, so a position loaded from a FEN knows where it stands.
  lazy val mustDraw: Boolean = isChaos && board.counterInPocket(player).isEmpty && !end
  lazy val canDraw: Boolean  = mustDraw

  lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues(_.map(_.dest)).to(Map)

  lazy val dropsAsDrops: List[Drop] = board.variant.validDrops(this)

  lazy val drops: Option[List[Pos]] =
    if (isChaos) Some(dropsAsDrops.map(_.pos)) else None

  lazy val canPass: Boolean = isOrder && !end

  // Order may always pass, so Order is never stuck; Chaos always has an empty square
  // while the board is not full
  lazy val canMove: Boolean = moves.nonEmpty

  def score = history.score

  def round: Int = board.round

  // The one action left to a player whose clock has run out. Deliberately not called
  // forcedAction: in backgammon that means "this action is inevitable, play it for me",
  // and a flagged entropy player still has legal alternatives they are barred from taking.
  def flaggedAction: Option[Action] =
    if (end) None
    else if (isOrder) pass().toOption
    else if (mustDraw) board.variant.randomDraw(this).toOption
    else
      for {
        role <- board.counterInPocket(player)
        pos  <- board.firstEmptyPosition
        d    <- drop(role, pos).toOption
      } yield d

  def move(from: Pos, to: Pos): Validated[String, Move] =
    board.variant.move(this, from, to)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest)

  def drop(role: Role, pos: Pos): Validated[String, Drop] =
    board.variant.drop(this, role, pos)

  def drawCounter(role: Role): Validated[String, DrawCounter] =
    board.variant.drawCounter(this, role)

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

  def withVariant(variant: strategygames.entropy.variant.Variant) =
    copy(board = board withVariant variant)

  def unary_! = copy(player = !player)
}

object Situation {

  def apply(variant: strategygames.entropy.variant.Variant): Situation =
    Situation(Board init variant, variant.startPlayer)

}
