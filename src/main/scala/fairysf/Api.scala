package strategygames.fairysf

import org.playstrategy.FairyStockfish

import cats.implicits._

import strategygames.{ Color, GameFamily, Pocket, Pockets }
import strategygames.fairysf.format.{ FEN, Uci }
import strategygames.fairysf.variant.Variant

sealed abstract class GameResult extends Product with Serializable

object GameResult {
  final case class Checkmate()  extends GameResult
  final case class Perpetual()  extends GameResult
  final case class Draw()       extends GameResult
  final case class VariantEnd() extends GameResult
  final case class Ongoing()    extends GameResult

  def resultFromInt(value: Int): GameResult =
    if (value.abs == 32000) GameResult.Checkmate()
    //else if (value == -32000) GameResult.VariantEnd()
    else if (value == 0) GameResult.Draw()
    else sys.error(s"Unknown game result: ${value}")

  def optionalResultFromInt(value: Int): GameResult =
    if (value == 32000) GameResult.Perpetual()
    //else if (value == 0) GameResult.Draw()
    else GameResult.Ongoing()

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

  private def pieceFromFSPiece(piece: FairyStockfish.Piece, gf: GameFamily): Piece =
    Piece(
      Color.all(piece.color()),
      if (piece.promoted) Role.promotionMap(Role.allByFairySFID(gf)(piece.pieceInfo().id))
      else Role.allByFairySFID(gf)(piece.pieceInfo().id)
    )

  def vectorOfPiecesToPieceArray(pieces: FairyStockfish.VectorOfPieces, gf: GameFamily): Array[Piece] =
    pieces.get().map(pieceFromFSPiece(_, gf))


  def convertPieceMap(fsPieceMap: FairyStockfish.PieceMap, gf: GameFamily): PieceMap = {
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
        Pos.fromKey(first.first().getString()).get
      ) = pieceFromFSPiece(first.second(), gf)
      first = first.increment()
    }
    pieceMap.toMap
  }

  def convertUciMoves(movesList: Option[List[String]]): Option[List[String]] =
    movesList.map(_.map(m => m match {
      case Uci.Move.moveP(orig, dest, promotion) => promotion match {
        case "" => m
        case _ => s"${orig}${dest}+"
      }
      case s: String => s
    }))

  // Actual API wrapper
  def version: String = FairyStockfish.version()

  def info(): Unit = FairyStockfish.info()

  def availableVariants(): Array[String] = FairyStockfish.availableVariants()

  def initialFen(variantName: String): FEN = FEN(FairyStockfish.initialFen(variantName))

  def validateFEN(variantName: String, fen: String): Boolean =
    FairyStockfish.validateFEN(variantName, fen)

  def fenFromMoves(variantName: String, fen: String, movesList: Option[List[String]] = None): FEN =
    FEN(FairyStockfish.getFEN(variantName, fen, convertUciMoves(movesList)))

  def gameResult(variantName: String, fen: String, movesList: Option[List[String]] = None): GameResult =
    if (legalMoves(variantName, fen, movesList).size == 0)
      GameResult.resultFromInt(
        FairyStockfish.gameResult(variantName, fen, convertUciMoves(movesList))
      )
    else optionalGameEndResult(variantName, fen, movesList)

  def gameEnd(variantName: String, fen: String, movesList: Option[List[String]] = None): Boolean =
    gameResult(variantName, fen, movesList) != GameResult.Ongoing() ||
      insufficientMaterial(variantName, fen, convertUciMoves(movesList)) == ((true, true))

  //def immediateGameEnd(variantName: String, fen: String, movesList: Option[List[String]] = None): Boolean =
  //  FairyStockfish.isImmediateGameEnd(variantName, fen, movesList).get0()

  private def isOptionalGameEnd(variantName: String, fen: String, movesList: Option[List[String]] = None) =
    FairyStockfish.isOptionalGameEnd(variantName, fen, convertUciMoves(movesList))

  def optionalGameEnd(variantName: String, fen: String, movesList: Option[List[String]] = None): Boolean =
    isOptionalGameEnd(variantName, fen, movesList).get0()

  def optionalGameEndResult(variantName: String, fen: String, movesList: Option[List[String]] = None): GameResult = {
    val optionalGameEndData = isOptionalGameEnd(variantName, fen, movesList)
    if (optionalGameEndData.get0()) GameResult.optionalResultFromInt(optionalGameEndData.get1())
    else GameResult.Ongoing()
  }

  def insufficientMaterial(variantName: String, fen: String, movesList: Option[List[String]] = None): (Boolean, Boolean) = {
    val im = FairyStockfish.hasInsufficientMaterial(variantName, fen, convertUciMoves(movesList))
    (im.get0(), im.get1())
  }

  def givesCheck(variantName: String, fen: String, movesList: Option[List[String]] = None): Boolean =
    FairyStockfish.givesCheck(variantName, fen, convertUciMoves(movesList))

  def legalMoves(variantName: String, fen: String, movesList: Option[List[String]] = None): Array[String] =
    FairyStockfish.getLegalMoves(variantName, fen, convertUciMoves(movesList))

  def piecesInHand(variantName: String, gf: GameFamily, fen: String): Array[Piece] =
    vectorOfPiecesToPieceArray(FairyStockfish.piecesInHand(variantName, fen), gf)

  def pocketData(variant: Variant, fen: String): Option[PocketData] =
    if (variant.dropsVariant){
      val pieces = piecesInHand(variant.fairysfName.name, variant.gameFamily, fen)
      PocketData(
        Pockets(
          Pocket(pieces.filter(_.color == White).toList.map(
            p => strategygames.Role.FairySFRole(p.role)
          )),
          Pocket(pieces.filter(_.color == Black).toList.map(
            p => strategygames.Role.FairySFRole(p.role)
          ))
        ),
        //Can make an empty Set of Pos because we dont have to track promoted pieces
        //(FairySF presumably does this)
        Set[Pos]()
      ).some
    } else None

  def pieceMapFromFen(variantName: String, gf: GameFamily, fen: String):PieceMap =
    convertPieceMap(FairyStockfish.piecesOnBoard(variantName, fen), gf)

}
