package draughts
package variant

case object Standard extends Variant(
  id = 1,
  gameType = 20,
  key = "standard",
  name = "Standard",
  shortName = "Std",
  title = "Standard rules of international draughts (FMJD).",
  standardInitialPosition = true,
  boardSize = Board.D100
) {
  import Variant._

  val pieces = symmetricFourRank(Vector(Man, Man, Man, Man, Man), boardSize)
  val initialFen = format.Forsyth.initial
  val startingPosition = StartingPosition("---", initialFen, "", "Initial position".some)

  val captureDirs: Directions = List((UpLeft, _.moveUpLeft), (UpRight, _.moveUpRight), (DownLeft, _.moveDownLeft), (DownRight, _.moveDownRight))
  val moveDirsColor: Map[Color, Directions] = Map(White -> List((UpLeft, _.moveUpLeft), (UpRight, _.moveUpRight)), Black -> List((DownLeft, _.moveDownLeft), (DownRight, _.moveDownRight)))
  val moveDirsAll: Directions = moveDirsColor(White) ::: moveDirsColor(Black)

  def maxDrawingMoves(board: Board): Option[Int] =
    drawingMoves(board, none).map(_._1)

  // (drawingMoves, first promotion: promotes this turn and has only one king)
  private def drawingMoves(board: Board, move: Option[Move]): Option[(Int, Boolean)] =
    if (board.pieces.size <= 4) {
      val whitePieces = board.pieces filter { p => !p._2.isGhost && p._2.is(Color.White) }
      val blackPieces = board.pieces filter { p => !p._2.isGhost && p._2.is(Color.Black) }
      val whiteKings = whitePieces.count(_._2.role == King)
      val blackKings = blackPieces.count(_._2.role == King)
      def firstPromotion = move.exists(m => m.promotes && m.color.fold(whiteKings == 1, blackKings == 1))
      val drawingMoves =
        if (whitePieces.size == 1 && whiteKings == 1) {
          if (blackKings == 0) 50
          else if (blackPieces.size <= 2) 10
          else 32
        } else if (blackPieces.size == 1 && blackKings == 1) {
          if (whiteKings == 0) 50
          else if (whitePieces.size <= 2) 10
          else 32
        } else 50
      Some(drawingMoves, drawingMoves != 50 && firstPromotion)
    } else Some(50, false)

  /**
   * Update position hashes for standard drawing rules:
   * - The game is drawn if three (or more) times the same position is repeated, with each time the same player having to move.
   * - The game is drawn when both players make 25 consecutive king moves without capturing.
   * - When one player has only a king left, and the other player three pieces including at least one king (three kings, two kings and a man, or one king and two men), the game is drawn after both players made 16 moves.
   * - When one player has only a king left, and the other player two pieces or less, including at least one king (one king, two kings, or one king and a man), the game is drawn after both players made 5 moves.
   */
  def updatePositionHashes(board: Board, move: Move, hash: draughts.PositionHash): PositionHash = {
    val newHash = Hash(Situation(board, !move.piece.color))
    drawingMoves(board, move.some) match {
      case Some((drawingMoves, firstPromotion)) =>
        if (drawingMoves == 50 && (move.captures || move.piece.isNot(King) || move.promotes))
          newHash // 25-move rule resets on capture or non-king move. promotion check is included to prevent that a move promoting a man is counted as a king move
        else {
          def piecesBefore = board.pieces.count(!_._2.isGhost) + Math.max(board.ghosts, move.taken.map(_.size).getOrElse(0))
          if (firstPromotion ||
            (drawingMoves == 10 && move.captures && piecesBefore > 3) ||
            (drawingMoves == 32 && move.captures && piecesBefore > 4))
            newHash // 16 and 5 move reset on promotion or capture that create the material situation, so that this move is not counted as the first
          else
            newHash ++ hash // 5 move rule never resets once activated
        }
      case _ => newHash
    }
  }
}
