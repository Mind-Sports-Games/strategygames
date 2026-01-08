package strategygames.dameo
package variant

import strategygames.dameo._
import strategygames.dameo.format.FEN
import strategygames.GameFamily

case object Dameo
    extends Variant(
      id = 1,
      key = "dameo",
      name = "Dameo",
      standardInitialPosition = true,
      boardSize = Board.Dim8x8
    ) {

  def gameFamily: GameFamily = GameFamily.Dameo()

  def perfIcon: Char = ''
  def perfId: Int    = 800

  override def baseVariant: Boolean = true

  override def initialFen: FEN = FEN(
    "W:Wa1,b1,b2,c1,c2,c3,d1,d2,d3,e1,e2,e3,f1,f2,f3,g1,g2,h1:Ba8,b7,b8,c6,c7,c8,d6,d7,d8,e6,e7,e8,f6,f7,f8,g7,g8,h8:H0:F1"
  )

  def maxDrawingMoves(board: Board): Option[Int] = drawingMoves(board)

  private def drawingMoves(board: Board): Option[Int] =
    if (board.kingVsKing()) { Some(4) }
    else { Some(50) }

  /** Update position hashes for standard drawing rules:
    *   - The game is drawn if three (or more) times the same position is repeated, with each time the same
    *     player having to move.
    *   - The game is drawn when both players make 25 consecutive king moves without capturing.
    *   - 4 moves have been player and its king vs king.
    */
  def updatePositionHashes(
      board: Board,
      move: Move,
      hash: PositionHash
  ): PositionHash = {
    val newHash = Hash(Situation(board, !move.piece.player))
    drawingMoves(board) match {
      case Some(drawingMoves) =>
        if (drawingMoves == 50 && (move.captures || move.piece.isNot(King) || move.promotes))
          newHash            // 25-move rule resets on capture or non-king move. promotion check is included to prevent that a move promoting
        else newHash ++ hash // 4 move rule never resets once activated
      case _                  => newHash
    }
  }
}
