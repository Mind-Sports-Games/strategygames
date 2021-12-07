package strategygames.fairysf

import strategygames.{ Color, Status }

import cats.data.Validated
import cats.implicits._

import strategygames.fairysf.format.{ Forsyth, Uci }

case class Situation(board: Board, color: Color) {

  lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  //lazy val playerCanCapture: Boolean = moves exists (_._2 exists (_.captures))

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _ map (_.dest) }.to(Map)

  def drops: Option[List[Pos]] = board.variant.possibleDrops(this)

  def dropsByRole: Option[Map[Role, List[Pos]]] = board.variant.possibleDropsByRole(this)

  //lazy val kingPos: Option[Pos] = board kingPosOf color

  lazy val check: Boolean = Api.givesCheck(
    board.variant.fairysfName.name,
    board.variant.initialFen.value,
    board.uciMoves.some
  )

  def checkSquare =
    if (check) board.posMap.get(Piece(color, board.variant.kingPiece)).flatMap(_.headOption)
    else None

  def history = board.history

  private def gameResult =
    if (Api.gameEnd(
      board.variant.fairysfName.name,
      board.variant.initialFen.value,
      board.uciMoves.some
    ))
      Api.gameResult(
        board.variant.fairysfName.name,
        board.variant.initialFen.value,
        board.uciMoves.some
      )
    else false

  def checkMate: Boolean = gameResult == GameResult.Checkmate()

  def perpetual: Boolean = gameResult == GameResult.Perpetual()

  def staleMate: Boolean = gameResult == GameResult.Draw() && Api.gameEnd(
    board.variant.fairysfName.name,
    board.variant.initialFen.value,
    board.uciMoves.some
  )

  private def variantEnd = gameResult == GameResult.VariantEnd()

  def end: Boolean = checkMate || perpetual || staleMate || variantEnd

  def winner: Option[Color] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean =
    (board valid strict) && !end && !copy(color = !color).check

  lazy val status: Option[Status] =
    if (checkMate) Status.Mate.some
    else if (perpetual) Status.PerpetualCheck.some
    //alot of variantEnds appear as checkMate in fairysf
    else if (variantEnd) Status.VariantEnd.some
    else if (staleMate) Status.Stalemate.some
    else none

  //TODO: test White/Black map is correct
  def opponentHasInsufficientMaterial: Boolean = {
    val insufficientMaterial = Api.insufficientMaterial(
      board.variant.fairysfName.name,
      board.variant.initialFen.value,
      board.uciMoves.some
    )
    color match {
      case White => insufficientMaterial._2
      case Black => insufficientMaterial._1
    }
  }

  //called threefold actually will return for xfold
  def threefoldRepetition: Boolean =
    Api.optionalGameEnd(
      board.variant.fairysfName.name,
      board.variant.initialFen.value,
      board.uciMoves.some
    )

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
