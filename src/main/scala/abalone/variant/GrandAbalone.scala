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
  override def perfIcon: Char = '\ue927'

  override def perfId: Int = 701

  override def maxUsable: Option[Int] = Option(4)

  override def winningScore: Int = 10

  override def hasPrevPlayer: Boolean = true

  /** The sequence of the number of actions per turn is 12* (P1 plays one move, then, starting with P2, both
    * players have two actions per turn).
    */
  override def turnCountFromFen(fenTurnCount: Int, player: Player): Int =
    fenTurnCount match {
      case t if t < 2 => player.fold(0, 1)
      case t          => 3 + 4 * (t - 2) + player.fold(0, 2)
    }

  /** The sequence of the number of actions per turn is 12* (P1 plays one move, then, starting with P2, both
    * players have two actions per turn).
    */
  override def isAutoEndTurn(situation: Situation, orig: Pos, dest: Pos): Boolean =
    situation.board.history.pliesRemainingThisTurn.fold(true)(_ < 2)

  override def finalizeBoardAfter_hist(move: Move): Board = {
    val res = super.finalizeBoardAfter_hist(move)
    res.history.pliesRemainingThisTurn = Option(if (move.autoEndTurn) 2 else 1)
    res
  }

  /** Belgian daisy. */
  override def initialFen: FEN =
    format.FEN("SS2ss/SSS1sss/1SS2ss1/9/ss6SS/sss5SSS/ss6SS/9/1SS2ss1/SSS1sss/SS2ss 0 0 b 0 1 1")
}
