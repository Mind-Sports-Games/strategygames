package strategygames.abalone

import strategygames.MoveMetrics
import strategygames.abalone.format.UUci
import strategygames.abalone.geometry.Cell

case class MMove(
                  piece: Piece,//TODO seems totally unnecessary
                  orig: Cell, dest: Cell,
                  situationBefore: SSituation,
                  after: BBoard,
                  autoEndTurn: Boolean,
                  capture: Option[Cell] = None,
                  metrics: MoveMetrics = MoveMetrics()
                ) extends AAction(situationBefore, after, metrics) {
  def withHistory(h: HHistory) = copy(after = after withHistory h)

  override def finalizeAfter: BBoard = {
    val board = after.updateHistory { h =>
      h.copy(
        lastTurn = if (autoEndTurn) h.currentTurn :+ toUci else h.lastTurn,
        currentTurn = if (autoEndTurn) List() else h.currentTurn :+ toUci,
        score = if (captures) h.score.add(situationBefore.player) else h.score,// This assumes the player cannot eject their own pieces
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
        lazy val positionHashesOfSituationBefore =
          if (h.positionHashes.isEmpty) Hash(situationBefore)
          else h.positionHashes
        val resetsPositionHashes = board.variant.isIrreversible(this)
        val basePositionHashes =
          if (resetsPositionHashes) Array.empty: PositionHash
          else positionHashesOfSituationBefore
        h.copy(positionHashes = Hash(SSituation(after, !piece.player)) ++ basePositionHashes)
      }
  }

  override def situationAfter = SSituation(finalizeAfter, if (autoEndTurn) !piece.player else piece.player)

  def applyVariantEffect: MMove = before.variant addVariantEffect this

  // does this move capture an opponent piece?
  def captures = capture.isDefined

  override def player = piece.player

  override def withMetrics(m: MoveMetrics) = copy(metrics = m)

  override def toUci = UUci.MMove(orig, dest)

  override def toString = s"$piece ${toUci.uci}"//TODO? why specify the piece? it only depends on the board
}
