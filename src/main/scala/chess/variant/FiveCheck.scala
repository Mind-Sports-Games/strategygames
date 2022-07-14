package strategygames.chess.variant

import strategygames.chess._
import strategygames.chess.format.FEN
import strategygames.Player

case object FiveCheck
    extends Variant(
      id = 12,
      key = "fiveCheck",
      name = "Five-check",
      standardInitialPosition = true
    ) {

  def pieces = Standard.pieces

  def perfId: Int    = 19
  def perfIcon: Char = '.'

  override val initialFen = FEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 5+5 0 1")

  override def finalizeBoard(board: Board, uci: format.Uci, capture: Option[Piece]): Board =
    board updateHistory {
      _.withCheck(Player.P1, board.checkP1).withCheck(Player.P2, board.checkP2)
    }

  override def specialEnd(situation: Situation) =
    situation.check && {
      val checks = situation.board.history.checkCount
      situation.player.fold(checks.p1, checks.p2) >= 5
    }

  /** It's not possible to check or checkmate the opponent with only a king
    */
  override def opponentHasInsufficientMaterial(situation: Situation) =
    situation.board.rolesOf(!situation.player) == List(King)

  // When there is insufficient mating material, there is still potential to win by checking the opponent 3 times
  // by the variant ending. However, no players can check if there are only kings remaining
  override def isInsufficientMaterial(board: Board) = board.pieces.values.forall(_ is King)
}
