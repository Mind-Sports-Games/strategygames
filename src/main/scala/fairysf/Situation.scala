package strategygames.fairysf

import strategygames.{ Color, Status }

import cats.data.Validated
import cats.implicits._

import strategygames.fairysf.format.{ Forsyth, Uci }

import org.playstrategy.FairyStockfish

case class Situation(board: Board, color: Color) {

  lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  //lazy val playerCanCapture: Boolean = moves exists (_._2 exists (_.captures))

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _ map (_.dest) }.to(Map)

  def drops: Option[List[Pos]] =
    board.variant match {
      //case v: variant.Shogi.type => v possibleDrops this
      case _                     => None
    }

  //lazy val kingPos: Option[Pos] = board kingPosOf color

  //stub
  lazy val check: Boolean = false

  //stub
  def checkSquare = None

  def history = board.history

  def checkMate: Boolean = board.variant checkmate this

  private def variantEnd = board.variant specialEnd this

  def end: Boolean = false //TODO: ???

  def winner: Option[Color] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean =
    (board valid strict) && !end && !copy(color = !color).check

  lazy val status: Option[Status] = None //TODO: ???

  //TODO: test White/Black map is correct
  def opponentHasInsufficientMaterial: Boolean = {
    val insufficientMaterial = FairyStockfish.hasInsufficientMaterial(
      board.variant.fairysfName.name,
      Forsyth.exportBoard(board),
      new FairyStockfish.VectorOfStrings()
    ) //TODO: ???
    color match {
      case White => insufficientMaterial.get0()
      case Black => insufficientMaterial.get1()
    }
  }

  def move(from: Pos, to: Pos, promotion: Option[PromotableRole]): Validated[String, Move] =
    board.variant.move(this, from, to, promotion)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest, uci.promotion)

  def drop(role: Role, pos: Pos): Validated[String, Drop] =
    board.variant.drop(this, role, pos)

  def withVariant(variant: strategygames.fairysf.variant.Variant) =
    copy(
      board = board withVariant variant
    )

  def unary_! = copy(color = !color)
}

object Situation {

  def apply(variant: strategygames.fairysf.variant.Variant): Situation = Situation(Board init variant, variant.startColor)
}
