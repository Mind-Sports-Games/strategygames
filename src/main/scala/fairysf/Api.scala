package strategygames.fairysf

import org.playstrategy.FairyStockfish

import cats.implicits._

import strategygames.{ GameFamily, Player, Pocket, Pockets }
import strategygames.fairysf.format.{ FEN, Uci }
import strategygames.fairysf.variant.Variant

sealed abstract class GameResult extends Product with Serializable

object GameResult {
  final case class Checkmate()  extends GameResult
  final case class Stalemate()  extends GameResult
  final case class Perpetual()  extends GameResult
  final case class Draw()       extends GameResult
  final case class VariantEnd() extends GameResult
  final case class Ongoing()    extends GameResult

  def resultFromInt(value: Int, check: Boolean): GameResult =
    if (value.abs == 32000)
      if (check) GameResult.Checkmate()
      else GameResult.Stalemate()
    else if (value == 0) GameResult.Draw()
    else sys.error(s"Unknown game result: ${value}")

  def optionalResultFromInt(value: Int): GameResult =
    if (value == 32000) GameResult.Perpetual()
    else GameResult.Ongoing()

}

object Api {
  // This will always be called when we import this module. So as long as we directly use
  // this package for calling everything, it should ensure that it's always initialized
  FairyStockfish.init()

  abstract class Position {
    val variant: Variant

    def makeMoves(movesList: List[String]): Position
    val fen: FEN
    val givesCheck: Boolean
    val isImmediateGameEnd: (Boolean, GameResult)
    val immediateGameEnd: Boolean
    val optionalGameEnd: Boolean
    val insufficientMaterial: (Boolean, Boolean)

    def isDraw(ply: Int): Boolean
    def hasGameCycle(ply: Int): Boolean
    val hasRepeated: Boolean

    val pieceMap: PieceMap
    val piecesInHand: Array[Piece]
    val pocketData: Option[PocketData]

    val optionalGameEndResult: GameResult
    val gameResult: GameResult
    val gameEnd: Boolean
    val legalMoves: Array[String]
  }

  private class FairyPosition(position: FairyStockfish.Position) extends Position {
    // TODO: yes, this is an abuse of scala. We could get an
    //       exception here, but I'm not sure how to work around that
    //       at the moment
    // NOTE: this means we can't use this API to test chess related things
    //       only the variants we support
    val variant = Variant.byFairySFName(position.variant())

    def makeMoves(movesList: List[String]): Position =
      if (movesList.isEmpty) this
      else new FairyPosition(position.makeMoves(movesList))

    lazy val fen: FEN            = FEN(position.getFEN())
    lazy val givesCheck: Boolean = position.givesCheck()
    lazy val isImmediateGameEnd: (Boolean, GameResult) = {
      val im = position.isImmediateGameEnd()
      (im.get0(), GameResult.resultFromInt(im.get1(), givesCheck))
    }
    lazy val immediateGameEnd: Boolean = isImmediateGameEnd._1
    private lazy val isOptionalGameEnd = position.isOptionalGameEnd()
    lazy val optionalGameEnd: Boolean  = isOptionalGameEnd.get0()
    lazy val insufficientMaterial: (Boolean, Boolean) = {
      val im = position.hasInsufficientMaterial()
      (im.get0(), im.get1())
    }

    def isDraw(ply: Int): Boolean       = position.isDraw(ply)
    def hasGameCycle(ply: Int): Boolean = position.hasGameCycle(ply)
    lazy val hasRepeated: Boolean       = position.hasRepeated()

    lazy val pieceMap: PieceMap =
      convertPieceMap(position.piecesOnBoard(), variant.gameFamily)

    lazy val piecesInHand: Array[Piece] =
      vectorOfPiecesToPieceArray(position.piecesInHand(), variant.gameFamily)

    lazy val pocketData =
      if (variant.dropsVariant)
        PocketData(
          Pockets(
            Pocket(
              piecesInHand.filter(_.player == P1).toList.map(p => strategygames.Role.FairySFRole(p.role))
            ),
            Pocket(
              piecesInHand.filter(_.player == P2).toList.map(p => strategygames.Role.FairySFRole(p.role))
            )
          ),
          //Can make an empty Set of Pos because we dont have to track promoted pieces
          //FairySF takes care of this for us
          Set[Pos]()
        ).some
      else None

    lazy val optionalGameEndResult: GameResult =
      if (isOptionalGameEnd.get0()) GameResult.optionalResultFromInt(isOptionalGameEnd.get1())
      else GameResult.Ongoing()

    lazy val gameResult: GameResult =
      if (legalMoves.size == 0)
        GameResult.resultFromInt(position.gameResult, givesCheck)
      else optionalGameEndResult

    lazy val gameEnd: Boolean =
      gameResult != GameResult.Ongoing() ||
        insufficientMaterial == ((true, true))

    lazy val legalMoves: Array[String] = position.getLegalMoves()
  }

  def positionFromVariant(variant: Variant): Position =
    new FairyPosition(new FairyStockfish.Position(variant.fairysfName.name))

  def positionFromVariantName(variantName: String): Position =
    new FairyPosition(new FairyStockfish.Position(variantName))

  def positionFromVariantNameAndFEN(variantName: String, fen: String): Position =
    new FairyPosition(new FairyStockfish.Position(variantName, fen))

  def positionFromVariantAndMoves(variant: Variant, uciMoves: List[String]): Position =
    positionFromVariant(variant).makeMoves(uciMoves)

  val emptyMoves = new FairyStockfish.VectorOfStrings()

  // Some implicits to make the below methods a bit more ergonomic
  implicit def bytePointerToString(bp: org.bytedeco.javacpp.BytePointer): String =
    bp.getString()

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

  implicit def intoArray(vos: FairyStockfish.VectorOfStrings): Array[String] =
    vos.get().map(_.getString())

  private def pieceFromFSPiece(piece: FairyStockfish.Piece, gf: GameFamily): Piece =
    Piece(
      Player.all(piece.color()),
      if (piece.promoted) Role.promotionMap(Role.allByFairySFID(gf)(piece.pieceInfo().id))
      else Role.allByFairySFID(gf)(piece.pieceInfo().id)
    )

  def vectorOfPiecesToPieceArray(pieces: FairyStockfish.VectorOfPieces, gf: GameFamily): Array[Piece] =
    pieces.get().map(pieceFromFSPiece(_, gf))

  private def convertPieceMap(fsPieceMap: FairyStockfish.PieceMap, gf: GameFamily): PieceMap = {
    var first    = fsPieceMap.begin()
    val pieceMap = scala.collection.mutable.Map[Pos, Piece]()
    while (!first.equals(fsPieceMap.end())) {
      pieceMap(
        Pos.fromKey(first.first().getString()).get
      ) = pieceFromFSPiece(first.second(), gf)
      first = first.increment()
    }
    pieceMap.toMap
  }

  private def convertUciMoves(movesList: Option[List[String]]): Option[List[String]] =
    movesList.map(
      _.map(m =>
        m match {
          case Uci.Move.moveP(orig, dest, promotion) =>
            promotion match {
              case "" => m
              case _  => s"${orig}${dest}+"
            }
          case s: String => s
        }
      )
    )

  // Actual API wrapper
  def version: String = FairyStockfish.version()

  def info(): Unit = FairyStockfish.info()

  def availableVariants(): Array[String] = FairyStockfish.availableVariants()

  def initialFen(variantName: String): FEN = FEN(FairyStockfish.initialFen(variantName))

  def validateFEN(variantName: String, fen: String): Boolean =
    FairyStockfish.validateFEN(variantName, fen)

  def positionFromMoves(variantName: String, fen: String, movesList: Option[List[String]] = None): Position =
    positionFromVariantNameAndFEN(variantName, fen)
      .makeMoves(convertUciMoves(movesList).getOrElse(List.empty))

  def pieceMapFromFen(variantName: String, fen: String): PieceMap =
    positionFromMoves(variantName, fen).pieceMap

}
