package strategygames.fairysf
package variant

import strategygames.GameFamily

import cats.implicits._

case object Xiangqi
    extends Variant(
      id = 2,
      key = "xiangqi",
      name = "Xiangqi",
      shortName = "Xiangqi",
      title = "Xiangqi (Chinese Chess)",
      standardInitialPosition = true
      //boardSize = Board.D100
    ) {
  import Variant._

  override def gameFamily: GameFamily = GameFamily.Xiangqi()

  def perfIcon: Char = 'K'
  def perfId: Int = 201

  override def baseVariant: Boolean = true

  val pieces: Map[Pos, Piece] = Variant.symmetricRank(backRank)
  //val initialFen = format.Forsyth.initial

}
