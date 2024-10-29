package strategygames.abalone
import strategygames.MoveMetrics

import strategygames.abalone.format.Uci

case class Move(
    piece: Piece,
    orig: Pos,
    dest: Pos,
    situationBefore: Situation,
    after: Board,
    capture: Option[Pos] = None,
    promotion: Option[PromotableRole] = None,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def withHistory(h: History) = copy(after = after withHistory h)

  override def finalizeAfter: Board = {
    val board = after.updateHistory({ h1 => h1.copy(
      lastTurn = h1.currentTurn :+ toUci,
      currentTurn = List(),
      score = if (captures) h1.score.add(situationBefore.player) else h1.score,
      halfMoveClock =
        if (captures) 0
        else h1.halfMoveClock + 1
    )
    })

    // Update position hashes last, only after updating the board.
    (board.variant.finalizeBoard(
      board,
      toUci,
      capture.flatMap(before.apply)
    ).updateHistory({ h =>
      lazy val positionHashesOfSituationBefore =
        if (h.positionHashes.isEmpty) Hash(situationBefore)
        else h.positionHashes
      val resetsPositionHashes                 = board.variant.isIrreversible(this)
      val basePositionHashes                   =
        if (resetsPositionHashes) Array.empty: PositionHash
        else positionHashesOfSituationBefore
      h.copy(positionHashes = Hash(Situation(after, !piece.player)) ++ basePositionHashes)
    }))
  }

  def situationAfter = Situation(finalizeAfter, !piece.player)

  def applyVariantEffect: Move = before.variant addVariantEffect this

  // does this move capture an opponent piece?
  def captures = capture.isDefined

  def promotes = promotion.isDefined

  def player = piece.player

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Move(orig, dest)

  override def toString = s"$piece ${toUci.uci}"
}
