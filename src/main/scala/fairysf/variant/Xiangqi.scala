package strategygames.fairysf
package variant

import strategygames.GameFamily

case object Xiangqi
    extends Variant(
      id = 2,
      key = "xiangqi",
      name = "Xiangqi",
      shortName = "Xiangqi",
      title = "Xiangqi (Chinese Chess)",
      standardInitialPosition = true,
      fairysfName=FairySFName("xiangqi"),
      boardSize = Board.Dim9x10
    ) {

  def gameFamily: GameFamily = GameFamily.Xiangqi()

  def perfIcon: Char = 't'
  def perfId: Int = 201

  override def baseVariant: Boolean = true

  val kingPiece: Role = XiangqiKing

  //cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN("rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C5C1/9/RNBAKABNR w - - 0 1")

  lazy val position: Api.Position = Api.positionFromVariantKey(this.key)
}
