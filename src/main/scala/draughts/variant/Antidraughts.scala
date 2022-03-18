package strategygames.draughts
package variant

import strategygames.Player

case object Antidraughts
    extends Variant(
      id = 6,
      gameType = 98,
      key = "antidraughts",
      name = "Antidraughts",
      standardInitialPosition = true,
      boardSize = Board.D100
    ) {

  def perfId: Int = 113

  def pieces           = Standard.pieces
  def initialFen       = Standard.initialFen
  def startingPosition = Standard.startingPosition

  def captureDirs    = Standard.captureDirs
  def moveDirsPlayer = Standard.moveDirsPlayer
  def moveDirsAll    = Standard.moveDirsAll

  // Only difference is that you win when you run out of moves (no pieces or all blocked)
  override def winner(situation: Situation): Option[Player] =
    if (situation.checkMate) Some(situation.player) else None

  def maxDrawingMoves(board: Board): Option[Int] = None

  /** Update position hashes for threefold repetition. Clear after non-kingmove, capture or promotion.
    */
  def updatePositionHashes(
      board: Board,
      move: Move,
      hash: strategygames.draughts.PositionHash
  ): PositionHash = {
    val newHash = Hash(Situation(board, !move.piece.player))
    if (move.captures || move.piece.isNot(King) || move.promotes) newHash else newHash ++ hash
  }

}
