package strategygames.fairysf
import org.playstrategy.FairyStockfish

sealed abstract class GameResult extends Product with Serializable

object GameResult {
  final case class Checkmate() extends GameResult
  final case class Draw()      extends GameResult

  def fromInt(value: Int): GameResult =
    if (value.abs == 32000) GameResult.Checkmate()
    else if (value.abs == 0) GameResult.Draw()
    else sys.error("Unknown game result")
}

object Api {
  // This will always be called when we import this module. So as long as we directly use
  // this package for calling everything, it should ensure that it's always initialized
  FairyStockfish.init();

  val emptyMoves = new FairyStockfish.VectorOfStrings()

  // Some implicits to make the below methods a bit more ergonomic
  implicit def bytePointerToString(bp: org.bytedeco.javacpp.BytePointer): String = bp.getString()
  implicit def vectorOfStringFromOptionalMovesList(
      movesList: Option[List[String]] = None
  ): FairyStockfish.VectorOfStrings =
    movesList.map(intoVector).getOrElse(emptyMoves)

  implicit def intoVector(l: List[String]): FairyStockfish.VectorOfStrings = {
    val vec = new FairyStockfish.VectorOfStrings()
    l.foreach(s => {
      vec.push_back(s);
    })
    vec
  }
  implicit def intoArray(vos: FairyStockfish.VectorOfStrings): Array[String] = vos.get().map(_.getString())


  // Actual API wrapper
  def version: String = FairyStockfish.version()
  def info(): Unit = FairyStockfish.info()
  def availableVariants(): Array[String] = FairyStockfish.availableVariants()
  def validateFEN(variantName: String, fen: String): Boolean = FairyStockfish.validateFEN(variantName, fen)
  def gameResult(variantName: String, fen: String, movesList: Option[List[String]] = None): GameResult =
    GameResult.fromInt(
      FairyStockfish.gameResult(variantName, fen, movesList)
    )
  def legalMoves(variantName: String, fen: String, movesList: Option[List[String]] = None): Array[String] =
    FairyStockfish.getLegalMoves(variantName, fen, movesList)

}
