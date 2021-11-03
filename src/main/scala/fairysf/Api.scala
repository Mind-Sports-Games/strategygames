package strategygames.fairysf

import org.playstrategy.FairyStockfish

import strategygames.Color
import strategygames.fairysf.format.FEN

sealed abstract class GameResult extends Product with Serializable

object GameResult {
  final case class Checkmate()  extends GameResult
  final case class Draw()       extends GameResult
  final case class VariantEnd() extends GameResult

  def fromInt(value: Int): GameResult =
    if (value.abs == 32000) GameResult.Checkmate()
    else if (value.abs == -32000) GameResult.VariantEnd()
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

  private def pieceFromFSPiece(piece: FairyStockfish.Piece): Piece =
    Piece(
      Color.all(piece.color()),
      Role.allByFairySFID(piece.pieceInfo().id)
    )

  implicit def vectorOfPiecesToPieceArray(pieces: FairyStockfish.VectorOfPieces): Array[Piece] =
    pieces.get().map(pieceFromFSPiece)


  implicit def convertPieceMap(fsPieceMap: FairyStockfish.PieceMap): PieceMap = {
    var first = fsPieceMap.begin()
    val pieceMap = scala.collection.mutable.Map[Pos, Piece]()
    //while(!first.equals(fsPieceMap.end())) {
    //  var temp = first.first().getString().pp("Pos: ")
    //  temp = first.second().pieceInfo().name().getString().pp("Piece")
    //  first = first.increment()
    //}
    //first = fsPieceMap.begin()
    while(!first.equals(fsPieceMap.end())) {
      pieceMap(
        Pos.fromKey(first.first().getString().pp("Pos:  ")).getOrElse(sys.error(s"Invalid Pos: ${first.first().getString()} Piece: ${first.second().pieceInfo().name().getString()}"))
      ) = pieceFromFSPiece(first.second()).pp("Piece:")
      first = first.increment()
    }
    pieceMap.toMap
  }


  // Actual API wrapper
  def version: String = FairyStockfish.version()

  def info(): Unit = FairyStockfish.info()

  def availableVariants(): Array[String] = FairyStockfish.availableVariants()

  def initialFen(variantName: String): FEN = FEN(FairyStockfish.initialFen(variantName))

  def validateFEN(variantName: String, fen: String): Boolean =
    FairyStockfish.validateFEN(variantName, fen)

  def fenFromMoves(variantName: String, fen: String, movesList: Option[List[String]] = None): FEN =
    FEN(FairyStockfish.getFEN(variantName, fen, movesList))

  def gameResult(variantName: String, fen: String, movesList: Option[List[String]] = None): GameResult =
    GameResult.fromInt(
      FairyStockfish.gameResult(variantName, fen, movesList)
    )

  def gameEnd(variantName: String, fen: String, movesList: Option[List[String]] = None): Boolean =
    legalMoves(variantName, fen, movesList).size == 0 || insufficientMaterial(variantName, fen, movesList) == ((true, true))

  //def immediateGameEnd(variantName: String, fen: String, movesList: Option[List[String]] = None): Boolean =
  //  FairyStockfish.isImmediateGameEnd(variantName, fen, movesList).get0()

  def optionalGameEnd(variantName: String, fen: String, movesList: Option[List[String]] = None): Boolean =
    FairyStockfish.isOptionalGameEnd(variantName, fen, movesList).get0()

  def insufficientMaterial(variantName: String, fen: String, movesList: Option[List[String]] = None): (Boolean, Boolean) = {
    val im = FairyStockfish.hasInsufficientMaterial(variantName, fen, movesList)
    (im.get0(), im.get1())
  }

  def legalMoves(variantName: String, fen: String, movesList: Option[List[String]] = None): Array[String] =
    FairyStockfish.getLegalMoves(variantName, fen, movesList)

  def piecesInHand(variantName: String, fen: String): Array[Piece] =
    FairyStockfish.piecesInHand(variantName, fen)

  def pieceMapFromFen(variantName: String, fen: String):PieceMap =
    FairyStockfish.piecesOnBoard(variantName, fen)

}
