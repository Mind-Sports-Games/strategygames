package strategygames.fairysf

import strategygames.{ GameMessage, Player, Status }

import cats.data.Validated
import cats.implicits._
import scalalib.extensions.*

import strategygames.fairysf.format.Uci

case class Situation(board: Board, player: Player) {

  lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _ map (_.dest) }.to(Map)

  def drops: Option[List[Pos]] = board.variant.possibleDrops(this)

  def dropsByRole: Option[Map[Role, List[Pos]]] = board.variant.possibleDropsByRole(this)

  def dropsAsDrops: List[Drop] = board.variant.validDrops(this)

  def canDrop: Boolean = dropsAsDrops.nonEmpty

  def canOnlyDrop: Boolean = canDrop && moves.isEmpty

  lazy val check: Boolean = board.apiPosition.givesCheck

  def checkSquare: Option[Pos] =
    board.variant.kingPiece.flatMap(kingPiece =>
      if (check) board.posMap.get(Piece(player, kingPiece)).flatMap(_.headOption)
      else None
    )

  def history = board.history

  private lazy val gameEnd: Boolean = board.apiPosition.gameEnd

  private lazy val gameResult: GameResult = board.apiPosition.gameResult

  private lazy val result =
    if (gameEnd) gameResult
    else GameResult.Ongoing()

  def checkMate: Boolean = result == GameResult.Checkmate()

  def perpetual: Boolean = result == GameResult.Perpetual()

  def staleMate: Boolean = result == GameResult.Stalemate()

  def autoDraw: Boolean = board.autoDraw || board.variant.specialDraw(this)

  private def variantEnd = result == GameResult.VariantEnd() || board.variant.specialEnd(this)

  def end: Boolean = checkMate || perpetual || staleMate || variantEnd || autoDraw

  def winner: Option[Player] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean = board.valid(strict) && !end && !(!this).check

  lazy val status: Option[Status] =
    if (checkMate) Status.Mate.some
    else if (perpetual) Status.PerpetualCheck.some
    // alot of variantEnds appear as checkMate in fairysf
    else if (variantEnd) Status.VariantEnd.some
    else if (staleMate) Status.Stalemate.some
    else if (autoDraw) Status.Draw.some
    else none

  // TODO: test P1/P2 map is correct
  def opponentHasInsufficientMaterial: Boolean = {
    val insufficientMaterial = board.apiPosition.insufficientMaterial
    player match {
      case P1 => insufficientMaterial._2
      case P2 => insufficientMaterial._1
    }
  }

  // called threefold actually will return for xfold
  def threefoldRepetition: Boolean =
    board.variant.repetitionEnabled && board.apiPosition.optionalGameEnd && !perpetualPossible

  lazy val perpetualPossible: Boolean =
    board.variant.validMoves(this).values.flatMap(_.map(_.situationAfter.perpetual)).toList.contains(true) ||
      board.variant.validDrops(this).map(_.situationAfter.perpetual).contains(true)

  lazy val gameMessage: Option[GameMessage] = perpetualPossible option GameMessage.PerpetualWarning

  def move(from: Pos, to: Pos, promotion: Option[PromotableRole]): Validated[String, Move] =
    board.variant.move(this, from, to, promotion)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest, uci.promotion)

  def drop(role: Role, pos: Pos): Validated[String, Drop] =
    board.variant.drop(this, role, pos)

  def drop(uci: Uci.Drop): Validated[String, Drop] =
    board.variant.drop(this, uci.role, uci.pos)

  def withVariant(variant: strategygames.fairysf.variant.Variant) =
    copy(
      board = board withVariant variant
    )

  // If we can't determine an inverse player the APIPosition is None and is then determined in
  // Board.apiPosition which uses uciMoves not FEN - will need to update when we have FromPosition
  def unary_! = copy(
    board = board.copy(
      position = board.variant
        .exportBoardFen(board)
        .invertPlayer
        .map(f =>
          Api.positionFromVariantNameAndFEN(
            board.variant.fishnetKey,
            f.value
          )
        )
    ),
    player = !player
  )
}

object Situation {

  def apply(variant: strategygames.fairysf.variant.Variant): Situation =
    Situation(Board init variant, variant.startPlayer)
}
