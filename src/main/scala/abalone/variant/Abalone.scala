package strategygames.abalone
package variant

import abalone.Hex5
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

  override def baseVariant: Boolean = true // Belgian Daisy initialFen is defined in Abalone default "Variant" file

  /** Belgian daisy. */
  //override def initialFen: FEN = FEN("ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss 0 0 b 0 1")
  override def initialFen: FEN = FEN("SS1ss/SSSsss/1SS1ss1/8/9/8/1ss1SS1/sssSSS/ss1SS 0 0 b 0 1")
}