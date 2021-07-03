package draughts

import cats.data.Validated
import cats.implicits._

import format.Uci

import scala.collection.breakOut
import scala.collection.mutable.ArrayBuffer

case class Situation(board: Board, color: Color) {

  lazy val actors = board actorsOf color

  lazy val ghosts = board.ghosts

  lazy val validMoves: Map[Pos, List[Move]] = board.variant.validMoves(this)
  lazy val validMovesFinal: Map[Pos, List[Move]] = board.variant.validMoves(this, true)

  lazy val allCaptures: Map[Pos, List[Move]] = actors.collect {
    case actor if actor.captures.nonEmpty =>
      actor.pos -> actor.captures
  }(breakOut)

  lazy val allMovesCaptureLength: Int =
    actors.foldLeft(0) {
      case (max, actor) =>
        Math.max(actor.captureLength, max)
    }

  def hasCaptures = actors.foldLeft(false) {
    case (capture, actor) =>
      if (capture) true
      else actor.captures.nonEmpty
  }

  def ambiguitiesMove(move: Move): Int = ambiguitiesMove(move.orig, move.dest)
  def ambiguitiesMove(orig: Pos, dest: Pos): Int = countAmbiguities(movesFrom(orig, true).filter(_.dest == dest))

  private def countAmbiguities(moves: List[Move]) =
    moves.foldLeft(0) {
      (total, m1) =>
        moves.exists { m2 =>
          m1 != m2 &&
            m1.dest == m2.dest &&
            m1.situationAfter.board.pieces != m2.situationAfter.board.pieces
        }.fold(total + 1, total)
    }

  def movesFrom(pos: Pos, finalSquare: Boolean = false): List[Move] = board.variant.validMovesFrom(this, pos, finalSquare)

  def captureLengthFrom(pos: Pos): Option[Int] =
    actorAt(pos).map(_.captureLength)

  lazy val allDestinations: Map[Pos, List[Pos]] = validMoves mapValues { _ map (_.dest) }
  lazy val allCaptureDestinations: Map[Pos, List[Pos]] = allCaptures mapValues { _ map (_.dest) }

  def destinationsFrom(pos: Pos, finalSquare: Boolean = false): List[Pos] = movesFrom(pos, finalSquare) map (_.dest)

  def validMoveCount = validMoves.foldLeft(0)((t, p) => t + p._2.length)

  def actorAt(pos: Pos): Option[Actor] = board.actorAt(pos)

  def drops: Option[List[Pos]] = None

  def history = board.history

  def checkMate: Boolean = board.variant checkmate this

  def autoDraw: Boolean = board.autoDraw || board.variant.specialDraw(this)

  lazy val threefoldRepetition: Boolean = board.history.threefoldRepetition

  def variantEnd = board.variant specialEnd this

  def end: Boolean = checkMate || autoDraw || variantEnd

  def winner: Option[Color] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean =
    (board valid strict) && !end

  lazy val status: Option[Status] =
    if (checkMate) Status.Mate.some
    else if (variantEnd) Status.VariantEnd.some
    else if (autoDraw) Status.Draw.some
    else none

  def move(from: Pos, to: Pos, promotion: Option[PromotableRole] = None, finalSquare: Boolean = false, forbiddenUci: Option[List[String]] = None, captures: Option[List[Pos]] = None, partialCaptures: Boolean = false): Validated[String, Move] =
    board.variant.move(this, from, to, promotion, finalSquare, forbiddenUci, captures, partialCaptures)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest, uci.promotion)

  def withHistory(history: DraughtsHistory) = copy(
    board = board withHistory history
  )

  def withVariant(variant: draughts.variant.Variant) = copy(
    board = board withVariant variant
  )

  def withoutGhosts = copy(
    board = board.withoutGhosts
  )

  def unary_! = copy(color = !color)
}

object Situation {

  def apply(variant: draughts.variant.Variant): Situation = Situation(Board init variant, White)

  def withColorAfter(board: Board, colorBefore: Color): Situation =
    if (board.ghosts == 0) Situation(board, !colorBefore)
    else Situation(board, colorBefore)
}
