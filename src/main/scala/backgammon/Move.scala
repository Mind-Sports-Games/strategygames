package strategygames.backgammon
import strategygames.MoveMetrics

import strategygames.backgammon.format.Uci

case class Move(
    piece: Piece,
    orig: Pos,
    dest: Pos,
    situationBefore: Situation,
    after: Board,
    capture: Option[Pos] = None,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore, after, metrics) {

  def situationAfter =
    Situation(finalizeAfter, situationBefore.player)

  def finalizeAfter: Board = after updateHistory { h =>
    h.copy(
      currentTurn = h.currentTurn :+ toUci,
      forcedTurn = h.forcedTurnPersists(situationBefore, this),
      justUsedUndo = false
    )
  }

  def lazySituationAfter =
    Situation(lazyFinalizeAfter, situationBefore.player)

  def lazyFinalizeAfter: Board = after updateHistory { h =>
    h.copy(
      currentTurn = h.currentTurn :+ toUci
    )
  }

  // this isn't really used for Backgammon and isnt defined for drop
  // but should work if uncommented and represent "does this move capture an opponent piece?"
  // def captures = capture.isDefined

  def captureList = capture.map(List(_))

  def diceUsed = (orig.index - dest.index).abs

  def player = piece.player

  def withMetrics(m: MoveMetrics) = copy(metrics = m)

  def toUci = Uci.Move(orig, dest, captureList)

  override def toString = s"$piece ${toUci.uci}"
}
