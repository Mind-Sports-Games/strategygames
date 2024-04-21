package strategygames.go
import strategygames.MoveMetrics

import strategygames.go.format.Uci

trait GenNextBoard {
  val boardAfter: Board
}

case class ExplicitBoardAfter(boardAfter: Board)    extends GenNextBoard
case class LazyBoardAfter(boardAfterF: () => Board) extends GenNextBoard {
  lazy val boardAfter = boardAfterF()
}

case class Drop(
    piece: Piece,
    pos: Pos,
    situationBefore: Situation,
    genNextBoard: GenNextBoard,
    autoEndTurn: Boolean,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore) {

  lazy val after = genNextBoard.boardAfter

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
