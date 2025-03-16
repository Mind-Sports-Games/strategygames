package strategygames.abalone
package variant

import strategygames.abalone.format.FEN

case object GrandAbalone extends Variant(
  id = 2,
  key = "grand_abalone",
  name = "Grand Abalone",
  standardInitialPosition = true,
  boardType = Hex6
) {
  override def perfIcon: Char = '\ue927'

  override def perfId: Int = 701

  override def maxUsable: Option[Int] = Option(4)

  override def winningScore: Int = 10

  /** Belgian daisy. */
  override def initialFen: FEN = format.FEN("SS2ss/SSS1sss/1SS2ss1/9/ss6SS/sss5SSS/ss6SS/9/1SS2ss1/SSS1sss/SS2ss 0 0 b 0 1")
}