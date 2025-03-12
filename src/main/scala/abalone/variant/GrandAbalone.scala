package strategygames.abalone
package variant

import strategygames.GameFamily
import strategygames.abalone.format.FEN

case object GrandAbalone extends Variant(
  id = 2,
  key = "grand_abalone",
  name = "Grand Abalone",
  standardInitialPosition = true,
  boardType = Board.Hex6
) {

  override def gameFamily: GameFamily = GameFamily.Abalone()

  override def perfIcon: Char = '\ue927'

  override def perfId: Int = 701

  override def winningScore: Int = 10

  override def baseVariant: Boolean = true

  /** Belgian daisy. */
  override def initialFen: FEN = format.FEN("ss2SS/sss1SSS/1ss2SS1/9/SS6ss/SSS5sss/SS6ss/9/1ss2SS1/sss1SSS/ss2SS 0 0 b 0 1")
}