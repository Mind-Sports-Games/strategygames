package strategygames.fiveinarow

import strategygames.{ Player, Status }

import cats.data.Validated

case class Situation(board: Board, player: Player) {

  def history = board.history

  def openingStep: OpeningStep = board.openingStep

  def blackSeat: Player = board.blackSeat

  def nextColour: Role = board.nextColour

  lazy val dropsAsDrops: List[Drop] = board.variant.validDrops(this)

  lazy val drops: Option[List[Pos]] = if (end) None else Some(dropsAsDrops.map(_.pos))

  lazy val canSwap: Boolean = board.variant.canSwap(this)

  lazy val canSwap2: Boolean = board.variant.canSwap2(this)

  def drop(role: Role, pos: Pos): Validated[String, Drop] = board.variant.drop(this, role, pos)

  def swap(): Validated[String, Swap] = board.variant.swap(this)

  def swap2(): Validated[String, Swap2] = board.variant.swap2(this)

  lazy val variantEnd: Boolean = board.variant.specialEnd(this)

  def end: Boolean = variantEnd

  def winner: Option[Player] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean = (board valid strict) && !end

  def opponentHasInsufficientMaterial: Boolean = false

  lazy val status: Option[Status] =
    if (variantEnd) Some(Status.VariantEnd)
    else None

  def withHistory(history: History) =
    copy(board = board withHistory history)

  def withVariant(variant: strategygames.fiveinarow.variant.Variant) =
    copy(board = board withVariant variant)

  def unary_! = copy(player = !player)
}

object Situation {

  def apply(variant: strategygames.fiveinarow.variant.Variant): Situation = {
    val board = Board init variant
    Situation(board, board.playerToMove)
  }

}
