package strategygames.go
import strategygames.MoveMetrics

import strategygames.go.format.Uci

case class Pass(
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) {

  private def before = situationBefore.board

  def situationAfter = Situation(finalizeAfter, !situationBefore.player)

  def finalizeAfter: Board = after updateHistory { h =>
    h.copy(
      lastMove = Option(toUci)
    )
  }

  def player = situationBefore.player

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Pass()

  override def toString = toUci.uci

}
