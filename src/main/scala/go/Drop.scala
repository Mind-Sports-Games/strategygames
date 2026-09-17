package strategygames.go
import strategygames.MoveMetrics

import strategygames.go.format.Uci

case class Drop(
    piece: Piece,
    pos: Pos,
    situationBefore: Situation,
    autoEndTurn: Boolean,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore) {

  // NOTE: `Variant.validDrops` builds one of these for every legal point and `possibleDrops` keeps only
  // their positions, so deferring the board keeps that call cheap. The cost is that an illegal
  // placement raises from the first read of `after`, and both construction sites check legality first.
  lazy val after: Board = before.variant.boardAfter(situationBefore, pos)

  def situationAfter =
    Situation(finalizeAfter, if (autoEndTurn) !piece.player else piece.player)

  def finalizeAfter: Board = after updateHistory { h =>
    h.copy(
      lastTurn = if (autoEndTurn) h.currentTurn :+ toUci else h.lastTurn,
      currentTurn = if (autoEndTurn) List() else h.currentTurn :+ toUci
    )
  }

  def applyVariantEffect: Drop = before.variant addVariantEffect this

  def player = piece.player

  def withMetrics(m: MoveMetrics): Drop = copy(metrics = m)

  def toUci: Uci.Drop = Uci.Drop(piece.role, pos)

  override def toString = toUci.uci

}
