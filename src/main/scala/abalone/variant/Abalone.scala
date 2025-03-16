package strategygames.abalone
package variant

import strategygames.GameFamily
import strategygames.abalone.format.FEN

case object Abalone extends Variant(
  id = 1,
  key = "abalone",
  name = "Abalone",
  standardInitialPosition = true,
  boardType = Hex5
) {
  override def gameFamily: GameFamily = GameFamily.Abalone()

  override def perfIcon: Char = '\ue927'

  override def perfId: Int = 700

  override def baseVariant: Boolean = true

  /** Belgian daisy. */
  override def initialFen: FEN = FEN("SS1ss/SSSsss/1SS1ss1/8/9/8/1ss1SS1/sssSSS/ss1SS 0 0 b 0 1")
}