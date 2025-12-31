package strategygames.go

import strategygames.{ GameMessage, Player, Status }
import strategygames.go.format.Forsyth

import cats.data.Validated
import cats.implicits._
import scalalib.extensions.*

case class Situation(board: Board, player: Player) {

  // lazy val moves: Map[Pos, List[Move]] = board.variant.validMoves(this)

  // lazy val destinations: Map[Pos, List[Pos]] = moves.view.mapValues { _ map (_.dest) }.to(Map)

  def drops: Option[List[Pos]] = board.variant.possibleDrops(this)

  def dropsByRole: Option[Map[Role, List[Pos]]] = board.variant.possibleDropsByRole(this)

  def dropsAsDrops: List[Drop] = board.variant.validDrops(this)

  def canDrop: Boolean = dropsAsDrops.nonEmpty

  def canOnlyDrop: Boolean = canDrop && !canSelectSquares

  def takebackable = !canSelectSquares

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

  def opponentHasInsufficientMaterial: Boolean = false

  def drop(role: Role, pos: Pos): Validated[String, Drop] =
    board.variant.drop(this, role, pos)

  def pass(): Validated[String, Pass] = board.variant.pass(this)

  def selectSquares(squares: List[Pos]): Validated[String, SelectSquares] =
    board.variant.selectSquares(this, squares)

  def canSelectSquares: Boolean =
    board.uciMoves.size > 1 && board.uciMoves
      .takeRight(2) == List("pass", "pass") && board.uciMoves.reverse.takeWhile(_ == "pass").length % 2 == 0

  def isSubsequentPassWarning: Boolean =
    board.uciMoves.size > 1 && board.uciMoves.takeRight(2) == List("pass", "pass")

  lazy val gameMessage: Option[GameMessage] =
    isSubsequentPassWarning option GameMessage.SubsequentPassWarning

  def withVariant(variant: strategygames.go.variant.Variant) =
    copy(
      board = board withVariant variant
    )

  // If we can't determine an inverse player the APIPosition is None and is then determined in
  // Board.apiPosition which uses uciMoves not FEN - will need to update when we have FromPosition
  def unary_! = copy(
    board = board.copy(
      position = Forsyth
        .exportBoardFen(board)
        .invertPlayer
        .map(f =>
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

  def apply(variant: strategygames.go.variant.Variant): Situation =
    Situation(Board init variant, variant.startPlayer)
}
