package strategygames.mancala

import com.joansala.game.oware.OwareGame

import cats.implicits._

import strategygames.mancala.variant.Variant

sealed abstract class GameResult extends Product with Serializable

object GameResult {
  final case class VariantEnd() extends GameResult
  final case class Draw()       extends GameResult
  final case class Ongoing()    extends GameResult

  def resultFromInt(value: Int, ended: Boolean): GameResult =
    if (value.abs == 1000) GameResult.VariantEnd()
    else if (value == 0 && ended) GameResult.Draw()
    else if (value == 0 && !ended) GameResult.Ongoing()
    else sys.error(s"Unknown game result: ${value}")

}

object Api {

  abstract class Position {
    val variant: Variant

    def makeMoves(movesList: List[Int]): Position
    def toBoard: String
    //val fen: FEN
    //val isImmediateGameEnd: (Boolean, GameResult)
    //val immediateGameEnd: Boolean
    //val optionalGameEnd: Boolean
    //val insufficientMaterial: (Boolean, Boolean)

    //def isDraw(ply: Int): Boolean
    //def hasGameCycle(ply: Int): Boolean
    //val hasRepeated: Boolean

    //val pieceMap: PieceMap
    //val piecesInHand: Array[Piece]

    //val optionalGameEndResult: GameResult
    val gameResult: GameResult
    val gameEnd: Boolean
    val legalMoves: Array[Int]
  }

  private class OwarePosition(position: OwareGame) extends Position {
    // TODO: yes, this is an abuse of scala. We could get an
    //       exception here, but I'm not sure how to work around that
    //       at the moment
    val variant = Variant.byKey("oware") //patch to get working not functional

    def makeMoves(movesList: List[Int]): Position = {
      movesList.map { move =>
        if (position.legalMoves.contains(move)) position.makeMove(move)
        else sys.error(s"Illegal move: ${move} from list: ${movesList}")
      }
      new OwarePosition(position)
    }

    //helper
    def toBoard: String = position.toBoard.toString

    //lazy val fen: FEN            = FEN(position.getFEN())

    //this is covered by gameEnd
    //lazy val isImmediateGameEnd: (Boolean, GameResult) = {
    //  val im = position.isImmediateGameEnd()
    //  (im.get0(), GameResult.resultFromInt(im.get1(), givesCheck))
    //}

    //lazy val immediateGameEnd: Boolean = position.hasEnded()

    //dont think there is any optional game end stuff here
    //private lazy val isOptionalGameEnd = position.isOptionalGameEnd()
    //lazy val optionalGameEnd: Boolean  = isOptionalGameEnd.get0()
    //lazy val insufficientMaterial: (Boolean, Boolean) = {
    //  val im = position.hasInsufficientMaterial()
    //  (im.get0(), im.get1())
    //}

    //def isDraw(ply: Int): Boolean       = position.isDraw(ply)
    //def hasGameCycle(ply: Int): Boolean = position.hasGameCycle(ply)
    //lazy val hasRepeated: Boolean       = position.hasRepeated()

    //lazy val pieceMap: PieceMap =
    //  convertPieceMap(position.piecesOnBoard(), variant.gameFamily)

    //lazy val piecesInHand: Array[Piece] =
    //  vectorOfPiecesToPieceArray(position.piecesInHand(), variant.gameFamily)

    //lazy val optionalGameEndResult: GameResult =
    //  if (isOptionalGameEnd.get0()) GameResult.optionalResultFromInt(isOptionalGameEnd.get1())
    //  else GameResult.Ongoing()

    lazy val gameResult: GameResult =
      GameResult.resultFromInt(position.outcome(), position.hasEnded())

    lazy val gameEnd: Boolean = position.hasEnded()

    val legalMoves: Array[Int] = {
      position.resetCursor()
      var moves: List[Int] = List()
      var nextMove         = position.nextMove()
      while (nextMove != -1) {
        moves = moves ::: List(nextMove)
        nextMove = position.nextMove()
      }
      moves.toArray
    }
  }

  def position: Position =
    new OwarePosition(new OwareGame())

//  def positionFromVariant(variant: Variant): Position =
//    new FairyPosition(new FairyStockfish.Position(variant.name))
//
//  def positionFromVariantName(variantName: String): Position =
//    new FairyPosition(new FairyStockfish.Position(variantName))
//
//  def positionFromVariantNameAndFEN(variantName: String, fen: String): Position =
//    new FairyPosition(new FairyStockfish.Position(variantName, fen))
//
//  def positionFromVariantAndMoves(variant: Variant, uciMoves: List[String]): Position =
//    positionFromVariant(variant).makeMoves(uciMoves)

//  def initialFen(variantName: String): FEN = FEN(FairyStockfish.initialFen(variantName))
//
//  def validateFEN(variantName: String, fen: String): Boolean =
//    FairyStockfish.validateFEN(variantName, fen)

//  def positionFromMoves(variantName: String, fen: String, movesList: Option[List[String]] = None): Position =
//    positionFromVariantNameAndFEN(variantName, fen)
//      .makeMoves(convertUciMoves(movesList).getOrElse(List.empty))
//
//  def pieceMapFromFen(variantName: String, fen: String): PieceMap =
//    positionFromMoves(variantName, fen).pieceMap

}
