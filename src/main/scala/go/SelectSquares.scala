package strategygames.go
import strategygames.MoveMetrics

import strategygames.go.format.Uci

case class SelectSquares(
    squares: List[Pos],
    situationBefore: Situation,
    autoEndTurn: Boolean,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore) {

  lazy val after: Board = before.variant.boardAfterSelectSquares(situationBefore, squares)

  def situationAfter =
    Situation(finalizeAfter, if (autoEndTurn) !situationBefore.player else situationBefore.player)

  def finalizeAfter: Board = after updateHistory { h =>
    h.copy(
      lastTurn = if (autoEndTurn) h.currentTurn :+ toUci else h.lastTurn,
      currentTurn = if (autoEndTurn) List() else h.currentTurn :+ toUci
    )
  }

  def applyVariantEffect = this

  def player = situationBefore.player

  def withMetrics(m: MoveMetrics): SelectSquares = copy(metrics = m)

  def toUci: Uci.SelectSquares = Uci.SelectSquares(squares)

  override def toString = toUci.uci

}
