package strategygames.go
import strategygames.MoveMetrics

import strategygames.go.format.Uci

trait NextBoard {
  val boardAfter: Board
}

case class ExplicitBoardAfter(boardAfter: Board)    extends NextBoard
case class LazyBoardAfter(boardAfterF: () => Board) extends NextBoard {
  lazy val boardAfter = boardAfterF()
}

case class Drop(
    piece: Piece,
    pos: Pos,
    situationBefore: Situation,
    nextBoard: NextBoard,
    autoEndTurn: Boolean,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore) {

  lazy val after = nextBoard.boardAfter

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

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Drop(piece.role, pos)

  override def toString = toUci.uci

}
