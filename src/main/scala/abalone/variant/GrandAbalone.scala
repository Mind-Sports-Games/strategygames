package strategygames.abalone
package variant

import strategygames.Player
import strategygames.abalone.format.FEN

case object GrandAbalone
    extends Variant(
      id = 2,
      key = "grandabalone",
      name = "Grand Abalone",
      standardInitialPosition = false,
      boardType = Hex6
    ) {
  override def perfIcon: Char = '\ue92C'

  override def perfId: Int = 701

  override def maxUsable: Option[Int] = Option(4)

  override def winningScore: Int = 10

  override def hasPrevPlayer: Boolean = true

  /** Grand Abalone has 2 plies per turn (vs 1 player-switch), so plies and
    * turnCount diverge. Overrides base implementation accordingly.
    */
  override def pliesFromFen(fenTurnCount: Int, player: Player, currentTurnPlies: Int = 0): Int =
    math.max(0, 2 * turnCountFromFen(fenTurnCount, player) - 1) + currentTurnPlies

  override def isAutoEndTurn(situation: Situation, orig: Pos, dest: Pos): Boolean =
    situation.board.history.pliesRemainingThisTurn.fold(true)(_ < 2)

  /** Check repetition only at complete turn boundaries (after ply 2). */
  override def repetition(situation: Situation): Boolean = {
    if (!repetitionEnabled) return false
    situation.board.history.lastAction.isDefined &&
    situation.board.history.pliesRemainingThisTurn.contains(2) &&
    situation.board.history.threefoldRepetition
  }

  override def finalizeBoardAfter_hist(move: Move): Board =
    super.finalizeBoardAfter_hist(move).updateHistory { h =>
      h.copy(
        pliesRemainingThisTurn = Option(if (move.autoEndTurn) 2 else 1)
      )
    }

  /** Belgian daisy. */
  override def initialFen: FEN =
    format.FEN("SS2ss/SSS1sss/1SS2ss1/9/ss6SS/sss5SSS/ss6SS/9/1SS2ss1/SSS1sss/SS2ss 0 0 b 0 1 1")
}
