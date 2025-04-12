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

  // TODO Dameo - might need to edit this, look at how draughts gamelogic does this (ghosts)
  def finalizeAfter: Board = {
    var newBoard = after.copy(pieces = after.pieces ++ capture.map(
      (_ -> after.pieces(capture.get).copy(role=after.pieces(capture.get).ghostRole))))
    .updateHistory({h =>
      h.copy(
        currentTurn = h.currentTurn :+ toUci
      )})
    if (promotion.nonEmpty) {
      newBoard = newBoard.copy(pieces = newBoard.pieces + (dest -> Piece(piece.player, promotion.get)))
    }
    if (autoEndTurn) {
      newBoard.copy(pieces = newBoard.pieces.filter({case (_,v) => !v.isGhost}))
    } else {
      newBoard
    }
  }

  def lazySituationAfter =
    Situation(lazyFinalizeAfter, situationBefore.player)

  def lazyFinalizeAfter: Board = after updateHistory { h =>
    h.copy(
      currentTurn = h.currentTurn :+ toUci
    )
  }

  def captures = capture.isDefined

  def promotes = promotion.isDefined

  def player = piece.player

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Move(orig, dest, promotion, capture)

  override def toString = s"$piece ${toUci.uci}"
}
