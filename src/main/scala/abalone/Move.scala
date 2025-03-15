package strategygames.abalone
import strategygames.MoveMetrics

import strategygames.abalone.format.Uci

@deprecated("Alex", since="1.5.5") case class Move(
    piece: Piece,
    orig: Pos,
    dest: Pos,
    situationBefore: Situation,
    after: Board,
    autoEndTurn: Boolean,
    capture: Option[Pos] = None,
    promotion: Option[PromotableRole] = None,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def withHistory(h: History) = copy(after = after withHistory h)

  override def finalizeAfter: Board = {
    val board = after.updateHistory { h =>
      h.copy(
        lastTurn = if (autoEndTurn) h.currentTurn :+ toUci else h.lastTurn,
        currentTurn = if (autoEndTurn) List() else h.currentTurn :+ toUci,
        score = if (captures) h.score.add(situationBefore.player) else h.score,
        halfMoveClock =
          if (captures) 0
          else h.halfMoveClock + 1
      )
    }

    // Update position hashes last, only after updating the board.
    board.variant
      .finalizeBoard(
        board,
        toUci,
        capture.flatMap(before.apply)
      )
      .updateHistory { h =>
        lazy val positionHashesOfSituationBefore =
          if (h.positionHashes.isEmpty) Hash(situationBefore)
          else h.positionHashes
        val resetsPositionHashes                 = board.variant.isIrreversible(this)
        val basePositionHashes                   =
          if (resetsPositionHashes) Array.empty: PositionHash
          else positionHashesOfSituationBefore
        h.copy(positionHashes = Hash(Situation(after, !piece.player)) ++ basePositionHashes)
      }
  }

  def situationAfter = Situation(finalizeAfter, if (autoEndTurn) !piece.player else piece.player)

  def applyVariantEffect: Move = before.variant addVariantEffect this

  // does this move capture an opponent piece?
  def captures = capture.isDefined

  def promotes = promotion.isDefined

  def player = piece.player

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Move(orig, dest)

  override def toString = s"$piece ${toUci.uci}"
}
