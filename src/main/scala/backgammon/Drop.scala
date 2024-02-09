package strategygames.backgammon
import strategygames.MoveMetrics

import strategygames.backgammon.format.Uci

case class Drop(
    piece: Piece,
    pos: Pos,
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def situationAfter =
    Situation(finalizeAfter, situationBefore.player)

  def finalizeAfter: Board = after updateHistory { h =>
    h.copy(
      currentTurn = h.currentTurn :+ toUci
    )
  }

  def player = piece.player

  private def beforeBoard = situationBefore.board

  def diceUsed = (Pos.barIndex(player) - pos.index).abs

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Drop(piece.role, pos)

  override def toString = toUci.uci

}
