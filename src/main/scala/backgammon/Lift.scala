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

  def playerAfter = if (endTurn) !player else player

  def situationAfter = Situation(finalizeAfter, playerAfter)

  def finalizeAfter: Board = afterWithScoreAndDice updateHistory { h =>
    h.copy(
      lastTurn = if (endTurn) h.currentTurn :+ toUci else h.lastTurn,
      currentTurn = if (endTurn) List() else h.currentTurn :+ toUci,
      forcedTurn = if (endTurn) false else h.forcedTurnPersists(situationBefore, this),
      justUsedUndo = false,
      halfMoveClock = if (endTurn) h.halfMoveClock + player.fold(0, 1) else h.halfMoveClock
    )
  }

  def lazySituationAfter =
    Situation(lazyFinalizeAfter, playerAfter)

  def lazyFinalizeAfter: Board = afterWithScore updateHistory { h =>
    h.copy(
      currentTurn = h.currentTurn :+ toUci
    )
  }

  // This version should work but it breaks forcedAction
  //def lazyFinalizeAfter: Board = afterWithScoreAndDice updateHistory { h =>
  //  h.copy(
  //    lastTurn = if (endTurn) h.currentTurn :+ toUci else h.lastTurn,
  //    currentTurn = if (endTurn) List() else h.currentTurn :+ toUci
  //  )
  //}

  // If we end the game we end the turn
  private lazy val endTurn = after.variant.endFromBoard(afterWithScore)

  private lazy val afterWithScore: Board = after updateHistory { h =>
    h.copy(
      score = Score(
        h.score.p1 + player.fold(1, 0),
        h.score.p2 + player.fold(0, 1)
      )
    )
  }

  private def afterWithScoreAndDice =
    if (endTurn) afterWithScore.setDice(List()) else afterWithScore

  def diceUsed = situationBefore.board.unusedDice.diff(after.unusedDice).head

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Lift(pos)

  override def toString = toUci.uci

}
