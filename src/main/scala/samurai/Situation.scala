package strategygames.samurai

import strategygames.{ Player, Status }

import cats.data.Validated
import cats.implicits._

import strategygames.samurai.format.Uci

case class Situation(board: Board, player: Player) {

  lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _ map (_.dest) }.to(Map)

  def history = board.history

  private lazy val gameEnd: Boolean = board.apiPosition.gameEnd

  private lazy val gameResult: GameResult = board.apiPosition.gameResult

  private lazy val result =
    if (gameEnd) gameResult
    else GameResult.Ongoing()

  // these dont exist in Oware. Normal ending tracked in VariantEnd
  def checkMate: Boolean = false
  def staleMate: Boolean = false

  private def variantEnd = result == GameResult.VariantEnd() || board.variant.specialEnd(this)

  def end: Boolean = checkMate || staleMate || variantEnd

  def winner: Option[Player] = board.variant.winner(this)

  def playable(strict: Boolean): Boolean =
    (board valid strict) && !end

  lazy val status: Option[Status] =
    if (checkMate) Status.Mate.some
    else if (variantEnd) Status.VariantEnd.some
    else if (staleMate) Status.Stalemate.some
    else none

  def isRepetition: Boolean = board.apiPosition.isRepetition

  def opponentHasInsufficientMaterial: Boolean =
    if (player == P1) board.apiPosition.fen.player1Score == 24 else board.apiPosition.fen.player2Score == 24

  def move(from: Pos, to: Pos, promotion: Option[PromotableRole]): Validated[String, Move] =
    board.variant.move(this, from, to, promotion)

  def move(uci: Uci.Move): Validated[String, Move] =
    board.variant.move(this, uci.orig, uci.dest, uci.promotion)

  def withVariant(variant: strategygames.samurai.variant.Variant) =
    copy(
      board = board withVariant variant
    )

  // If we can't determine an inverse player the APIPosition is None and is then determined in
  // Board.apiPosition which uses uciMoves not FEN - will need to update when we have FromPosition
  def unary_! = copy(
    board = board.copy(
      position = board.apiPosition.fen.invertPlayer.map(f =>
        Api.positionFromVariantNameAndFEN(
          board.variant.key,
          f.value
        )
      )
    ),
    player = !player
  )
}

object Situation {

  def apply(variant: strategygames.samurai.variant.Variant): Situation =
    Situation(Board init variant, variant.startPlayer)
}
