package strategygames
import ornicar.scalalib
import org.playstrategy.FairyStockfish;

package object fairysf extends scalalib.Common with scalalib.OrnicarOption with scalalib.OrnicarBoolean {

  val White = strategygames.Color.White
  val Black = strategygames.Color.Black

  type Direction  = Pos => Option[Pos]
  type Directions = List[Direction]

  type PieceMap = Map[Pos, Piece]

  type PositionHash = Array[Byte]

  type MoveOrDrop = Either[Move, Drop]

  // This needs to be called at least once, before everything else.
  FairyStockfish.init();

}
