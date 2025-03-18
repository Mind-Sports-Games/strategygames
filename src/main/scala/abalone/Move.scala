package strategygames.abalone

import strategygames.abalone.format.Uci
import strategygames.{MoveMetrics, Player}

case class Move(
                 player: Player,
                 orig: Pos, dest: Pos,
                 situationBefore: Situation,
                 after: Board,
                 autoEndTurn: Boolean,
                 capture: Option[Pos] = None,
                 metrics: MoveMetrics = MoveMetrics()
               ) extends Action(situationBefore, after, metrics) {
  def withHistory(h: History) = copy(after = after withHistory h)

  override def finalizeAfter: Board = {
    val board = after.updateHistory { h =>
      h.copy(
        lastTurn = if (autoEndTurn) h.currentTurn :+ toUci else h.lastTurn,
        currentTurn = if (autoEndTurn) List() else h.currentTurn :+ toUci,
        prevPlayer = Option(player),
        prevMove = Option(this),
        score = if (captures) h.score.add(player) else h.score, // This assumes the player cannot eject their own pieces
        halfMoveClock = if (captures) 0 else h.halfMoveClock + 1
      )
    }

    // Update position hashes last, only after updating the board.
    board.variant
      .finalizeBoard(
        board,
        toUci,
        capture.flatMap(before.getPiece)
      )
      .updateHistory { h =>
        val prevPositionHashes =
          if (board.variant.isIrreversible(this)) Array.empty: PositionHash
          else if (h.positionHashes.isEmpty) Hash(situationBefore)
          else h.positionHashes

        h.copy(positionHashes =
          if (autoEndTurn) Hash(Situation(after, player)) ++ prevPositionHashes
          else prevPositionHashes
        )
      }
  }

  override def situationAfter = Situation(finalizeAfter, if (autoEndTurn) !situationBefore.player else situationBefore.player)

  def applyVariantEffect: Move = before.variant addVariantEffect this

  // does this move capture an opponent piece?
  def captures = capture.isDefined

  override def withMetrics(m: MoveMetrics) = copy(metrics = m)

  override def toUci = Uci.Move(orig, dest)

  override def toString = s"${player.number} ${toUci.uci}"
}