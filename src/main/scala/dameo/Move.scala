package strategygames.dameo
import strategygames.MoveMetrics

import strategygames.dameo.format.Uci

case class Move(
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

  def situationAfter =
    Situation(
      finalizeAfter,
      if (autoEndTurn) !piece.player else piece.player
    )

  def finalizeAfter: Board = {
    var newBoard = after.copy(pieces =
      after.pieces ++ capture.map(
        (_ -> after.pieces(capture.get).copy(role = after.pieces(capture.get).ghostRole))
      )
    )
    if (promotion.nonEmpty) {
      newBoard = newBoard.copy(pieces = newBoard.pieces + (dest -> Piece(piece.player, promotion.get)))
    }
    newBoard = if (autoEndTurn) {
      newBoard.copy(pieces =
        newBoard.pieces.filter { case (_, v) =>
          !v.isGhost
        } +
          (dest -> newBoard.pieces(dest).copy(role = newBoard.pieces(dest).unactiveRole))
      )
    } else {
      newBoard.copy(pieces = newBoard.pieces + (dest -> Piece(piece.player, piece.activeRole)))
    }
    newBoard.updateHistory { h =>
      h.copy(
        currentTurn = h.currentTurn :+ toUci,
        halfMoveClock = h.halfMoveClock + (if (autoEndTurn && newBoard.kingVsKing()) 1 else 0),
        positionHashes =
          if (autoEndTurn) newBoard.variant.updatePositionHashes(newBoard, this, h.positionHashes)
          else h.positionHashes
      )
    }
  }

  def captures = capture.isDefined

  def promotes = promotion.isDefined

  def player = piece.player

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Move(orig, dest, promotion, capture)

  override def toString = s"$piece ${toUci.uci}"
}
