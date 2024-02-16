package strategygames.backgammon

import strategygames.{ Player, Status }

import cats.data.Validated
import cats.implicits._

import strategygames.backgammon.format.Uci

case class Situation(board: Board, player: Player) {

  lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _ map (_.dest) }.to(Map)

  def drops: Option[List[Pos]] = board.variant.possibleDrops(this)

  // Do we need this - role is always the same for Backgammon
  def dropsByRole: Option[Map[Role, List[Pos]]] = board.variant.possibleDropsByRole(this)

  def dropsAsDrops: List[Drop] = board.variant.validDrops(this)

  def lifts: List[Lift] = board.variant.validLifts(this)

  def diceRolls: List[DiceRoll] = board.variant.validDiceRolls(this)

  def canMove: Boolean = moves.nonEmpty

  def canDrop: Boolean = dropsAsDrops.nonEmpty

  // In Backgammon when we can drop we have to drop - we can't do anything else
  def canOnlyDrop: Boolean = canDrop

  def canLift: Boolean = lifts.nonEmpty

  def canOnlyLift: Boolean = canLift && !canMove && !canDrop

  def canRollDice: Boolean = diceRolls.nonEmpty

  def canOnlyRollDice: Boolean = canRollDice && !canMove && !canDrop && !canLift && !canEndTurn

  def canEndTurn: Boolean = board.variant.validEndTurn(this).nonEmpty

  def canOnlyEndTurn: Boolean = canEndTurn && !canMove && !canDrop && !canLift

  def canUseDice: Boolean = board.unusedDice.nonEmpty && (canMove || canDrop || canLift)

  def canUndo: Boolean = board.history.lastAction.map(_.undoable).getOrElse(false)

  def history = board.history

  def end: Boolean = board.variant.specialEnd(this)

  def winner: Option[Player] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean =
    (board valid strict) && !end

  lazy val status: Option[Status] =
    if (board.variant.backgammonWin(this)) Status.BackgammonWin.some
    else if (board.variant.gammonWin(this)) Status.GammonWin.some
    else if (end) Status.SingleWin.some
    else none

  def opponentHasInsufficientMaterial: Boolean =
    if (player == P1) (board.history.score.p1 == 81) else (board.history.score.p2 == 81)

  def move(from: Pos, to: Pos): Validated[String, Move] =
    board.variant.move(this, from, to)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest)

  def drop(role: Role, pos: Pos): Validated[String, Drop] =
    board.variant.drop(this, role, pos)

  def lift(pos: Pos): Validated[String, Lift] =
    board.variant.lift(this, pos)

  def diceRoll(dice: List[Int]): Validated[String, DiceRoll] =
    board.variant.diceRoll(this, dice)

  def endTurn(): Validated[String, EndTurn] = board.variant.endTurn(this)

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
