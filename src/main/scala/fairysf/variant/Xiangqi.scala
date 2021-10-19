package strategygames.fairysf
package variant

import strategygames.GameFamily
import strategygames.fairysf.format.FEN
import org.playstrategy.FairyStockfish

import cats.implicits._

case object Xiangqi
    extends Variant(
      id = 2,
      key = "xiangqi",
      name = "Xiangqi",
      shortName = "Xiangqi",
      title = "Xiangqi (Chinese Chess)",
      standardInitialPosition = true,
      fairysfName=FairySFName("xiangqi")
      //boardSize = Board.D100
    ) {
  import Variant._

  FairyStockfish.init()

  override def gameFamily: GameFamily = GameFamily.Xiangqi()

  def perfIcon: Char = 't'
  def perfId: Int = 201

  override def baseVariant: Boolean = true

  val pieces: Map[Pos, Piece] = Map.empty() //TODO: ???
  //override val initialFen = FEN(
  //  FairyStockfish.initialFen(fairysfName.name)
  //)
  // FEN("rnbakabnr/9/1c5c1/p1p1p1p1p/9/9/P1P1P1P1P/1C2C4/9/RNBAKABNR b - - 1 1")

}
