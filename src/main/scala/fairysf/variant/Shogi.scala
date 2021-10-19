package strategygames.fairysf
package variant

import cats.implicits._
import cats.syntax.option._
import cats.data.Validated

import strategygames.fairysf._
import strategygames.fairysf.format.Uci
import strategygames.{ Color, GameFamily }

import org.playstrategy.FairyStockfish

case object Shogi
    extends Variant(
      id = 1,
      key = "shogi",
      name = "Shogi",
      shortName = "Shogi",
      title = "Shogi (Japanese Chess)",
      standardInitialPosition = true,
      fairysfName=FairySFName("shogi")
      //boardSize = Board.D100
    ) {
  import Variant._

  FairyStockfish.init()
  
  override def gameFamily: GameFamily = GameFamily.Shogi()

  override def dropsVariant = true

  def perfIcon: Char = 's'
  def perfId: Int = 200

  override def baseVariant: Boolean = true

  val pieces: Map[Pos, Piece] = Map.empty()//TODO: ???

}
