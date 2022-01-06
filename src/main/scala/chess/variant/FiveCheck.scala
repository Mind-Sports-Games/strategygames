package strategygames.chess.variant

import strategygames.chess._
import strategygames.chess.format.FEN
import strategygames.Color

case object FiveCheck
    extends Variant(
      id = 12,
      key = "fiveCheck",
      name = "Five-check",
      shortName = "5check",
      title = "Check your opponent 5 times to win the game.",
      standardInitialPosition = true
    ) {

  def pieces = Standard.pieces

  def perfId: Int    = 15
  def perfIcon: Char = '.'

  override def aiVariant: Boolean   = false
  override val initialFen = FEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 +0+0")

  override def finalizeBoard(board: Board, uci: format.Uci, capture: Option[Piece]): Board =
    board updateHistory {
      _.withCheck(Color.White, board.checkWhite).withCheck(Color.Black, board.checkBlack)
    }

  override def specialEnd(situation: Situation) =
    situation.check && {
      val checks = situation.board.history.checkCount
      situation.color.fold(checks.white, checks.black) >= 5
    }

  /** It's not possible to check or checkmate the opponent with only a king
    */
  override def opponentHasInsufficientMaterial(situation: Situation) =
    situation.board.rolesOf(!situation.color) == List(King)

  // When there is insufficient mating material, there is still potential to win by checking the opponent 3 times
  // by the variant ending. However, no players can check if there are only kings remaining
  override def isInsufficientMaterial(board: Board) = board.pieces.values.forall(_ is King)
}