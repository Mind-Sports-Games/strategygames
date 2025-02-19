package strategygames.backgammon
import strategygames.{ MoveMetrics, Score }

import strategygames.backgammon.format.Uci

case class Lift(
    pos: Pos,
    situationBefore: Situation,
    after: Board,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def player = situationBefore.player

  def situationAfter = {
    val s = Situation(finalizeAfter, player)
    // s.end needs to be called on a situation but who is set as player is irrelevant as
    // s.end doesn't check player. but this enables us to know who the player in the
    // situationAfter should be
    if (s.end) s.copy(player = !player)
    else s
  }

  def finalizeAfter: Board = after updateHistory { h =>
    h.copy(
      currentTurn = h.currentTurn :+ toUci,
      forcedTurn = h.forcedTurnPersists(situationBefore, this),
      justUsedUndo = false,
      score = Score(
        h.score.p1 + player.fold(1, 0),
        h.score.p2 + player.fold(0, 1)
      )
    )
  }

  def lazySituationAfter =
    Situation(lazyFinalizeAfter, situationBefore.player)

  def lazyFinalizeAfter: Board = after updateHistory { h =>
    h.copy(
      currentTurn = h.currentTurn :+ toUci,
      score = Score(
        h.score.p1 + player.fold(1, 0),
        h.score.p2 + player.fold(0, 1)
      )
    )
  }

  def diceUsed = situationBefore.board.unusedDice.diff(after.unusedDice).head

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Lift(pos)

  override def toString = toUci.uci

}
