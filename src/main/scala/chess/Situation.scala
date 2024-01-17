package strategygames.chess

import strategygames.{ Player, Status }

import cats.data.Validated
import cats.implicits._

import strategygames.chess.format.Uci

case class Situation(board: Board, player: Player) {

  lazy val actors = board actorsOf player

  lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  lazy val playerCanCapture: Boolean = moves exists (_._2 exists (_.captures))

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _ map (_.dest) }.to(Map)

  def drops: Option[List[Pos]] =
    board.variant match {
      case v: variant.Crazyhouse.type => v possibleDrops this
      case _                          => None
    }

  def dropsByRole: Option[Map[Role, List[Pos]]] =
    board.variant match {
      case v: variant.Crazyhouse.type => v possibleDropsByRole this
      case _                          => None
    }

  def canRollDice: Boolean = board.variant.validDiceRolls(this).nonEmpty

  lazy val kingPos: Option[Pos] = board kingPosOf player

  lazy val check: Boolean = board check player

  def checkSquare = if (check) kingPos else None

  def history = board.history

  def checkMate: Boolean = board.variant checkmate this

  def staleMate: Boolean = board.variant staleMate this

  def autoDraw: Boolean = board.autoDraw || board.variant.specialDraw(this)

  def opponentHasInsufficientMaterial: Boolean = board.variant.opponentHasInsufficientMaterial(this)

  lazy val threefoldRepetition: Boolean = board.history.threefoldRepetition

  def variantEnd = board.variant specialEnd this

  def end: Boolean = checkMate || staleMate || autoDraw || variantEnd

  def winner: Option[Player] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean =
    (board valid strict) && !end && !copy(player = !player).check

  lazy val status: Option[Status] =
    if (checkMate) Status.Mate.some
    else if (variantEnd) Status.VariantEnd.some
    else if (staleMate) Status.Stalemate.some
    else if (autoDraw) Status.Draw.some
    else none

  def move(from: Pos, to: Pos, promotion: Option[PromotableRole]): Validated[String, Move] =
    board.variant.move(this, from, to, promotion)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest, uci.promotion)

  def drop(role: Role, pos: Pos): Validated[String, Drop] =
    board.variant.drop(this, role, pos)

  def diceRoll(dice: List[Int]): Validated[String, DiceRoll] =
    board.variant.diceRoll(this, dice)

  def fixCastles = copy(board = board fixCastles)

  def withHistory(history: History) =
    copy(
      board = board withHistory history
    )

  def withVariant(variant: strategygames.chess.variant.Variant) =
    copy(
      board = board withVariant variant
    )

  def canCastle = board.history.canCastle _

  // multiple squares are enpassantable in multiaction variants
  def enPassantSquares: List[Pos] = board.variant.enPassantSquares(this)

  def enPassantSquaresUciString: Option[String] =
    if (enPassantSquares.nonEmpty) Some(enPassantSquares.map(_.toString).mkString(","))
    else None

  def lastActionOfTurn: Boolean = board.variant.lastActionOfTurn(this)

  def unary_! = copy(player = !player)
}

object Situation {

  def apply(variant: strategygames.chess.variant.Variant): Situation =
    Situation(Board init variant, variant.startPlayer)
}
