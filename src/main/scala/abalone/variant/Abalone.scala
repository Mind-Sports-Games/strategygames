package strategygames.abalone
package variant

import strategygames.abalone.format.FEN

case object Abalone
    extends Variant(
      id = 1,
      key = "abalone",
      name = "Abalone",
      standardInitialPosition = true,
      boardType = Hex5
    ) {
  override def perfIcon: Char = '\ue927'

  override def perfId: Int = 700

  override def baseVariant: Boolean = true

  /** Belgian daisy. */
  override def initialFen: FEN = FEN("ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss 0 0 b 0 1")
}
