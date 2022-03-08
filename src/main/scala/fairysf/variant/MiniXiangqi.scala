package strategygames.fairysf
package variant

import strategygames.GameFamily

case object MiniXiangqi
    extends Variant(
      id = 4,
      key = "minixiangqi",
      name = "Mini Xiangqi",
      standardInitialPosition = true,
      fairysfName = FairySFName("minixiangqi"),
      boardSize = Board.Dim7x7
    ) {

  def gameFamily: GameFamily = GameFamily.Xiangqi()

  def perfIcon: Char = 't'
  def perfId: Int    = 203

  override val kingPiece: Option[Role] = Some(XiangqiKing)

  //cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN("rcnkncr/p1ppp1p/7/7/7/P1PPP1P/RCNKNCR w - - 0 1")

}
