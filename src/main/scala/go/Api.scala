package strategygames.go

import scala.annotation.nowarn

import strategygames.{ Pocket, Pockets, Score }
import strategygames.go.format.FEN
import strategygames.go.Pos
import strategygames.go.variant.Variant

sealed abstract class GameResult extends Product with Serializable

object GameResult {
  final case class VariantEnd() extends GameResult
  final case class Draw()       extends GameResult
  final case class Ongoing()    extends GameResult

  def resultFromInt(value: Int, ended: Boolean, isRepetition: Boolean): GameResult =
    if (value.abs == 1000 && ended) GameResult.VariantEnd()
    else if (value == 0 && ended && isRepetition) GameResult.VariantEnd() // e.g. repeating 3 ko's
    else if (value == 0 && ended) GameResult.Draw()
    else if (!ended) GameResult.Ongoing()
    else sys.error(s"Unknown game result: ${value}")

}

object Api {

  abstract class Position {
    lazy val variant: Variant

    // todo rename moves to actions to be consistent
    def makeMoves(movesList: List[String]): Position
    private[go] def makeMovesNoLegalCheck(movesList: List[String]): Position
    def makeMovesWithPrevious(
        movesList: List[String],
        previousMoves: List[String]
    ): Position
    def createPosFromPrevious(previousMoves: List[String]): Position
    private[go] def makeMovesWithPosUnchecked(
        movesList: List[String],
        posWithPrevious: Position
    ): Position

    def setKomi(komi: Double): Unit
    def deepCopy: Position

    lazy val turn: String
    lazy val initialFen: FEN
    lazy val fen: FEN
    lazy val pieceMap: PieceMap
    lazy val pocketData: Option[PocketData]

    lazy val fenScore: Score
    lazy val gameResult: GameResult
    lazy val gameEnd: Boolean
    def gameOutcome: Int
    lazy val isRepetition: Boolean
    def gameScore: Int
    def p1Score: Double
    def p2Score: Double
    lazy val legalDrops: Array[Int]
    lazy val legalActions: Array[Int]
    lazy val playerTurn: Int // 1 for South (P1/black) -1 for North (P2/white)
    def fenString: String
  }

  private[go] val stonePocketData: Option[PocketData] =
    Some(
      PocketData(
        Pockets(
          Pocket(List(strategygames.Role.GoRole(Stone), strategygames.Role.GoRole(Stone))),
          Pocket(List(strategygames.Role.GoRole(Stone), strategygames.Role.GoRole(Stone)))
        )
      )
    )

  def position(variant: Variant, komi: Double = 7.5): Position =
    ScalaPosition.initial(variant, komi)

  def positionFromVariant(variant: Variant): Position = position(variant, variant.komi)

  def positionFromVariantNameAndFEN(variantKey: String, fenString: String): Position = {
    val positionFen = FEN(fenString)
    val variant     = Variant(variantKey)
      .filter(_.boardSize.height == positionFen.gameSize)
      .getOrElse(sys.error(s"incorrect variant name (${variantKey}) and/or fen (${positionFen})"))
    ScalaPosition.fromFen(variant, positionFen)
  }

  def positionFromVariantAndMoves(variant: Variant, uciMoves: List[String]): Position =
    ScalaPosition.batchFromInitial(variant, uciMoves)

  def positionFromVariantStartingFenAndMoves(
      variant: Variant,
      startingFen: FEN,
      uciMoves: List[String]
  ): Position =
    ScalaPosition.batchFromFen(variant, startingFen, uciMoves)

  def positionsFromVariantStartingFenAndMoves(
      variant: Variant,
      startingFen: FEN,
      uciMoves: List[String]
  ): Vector[Position] =
    ScalaPosition.positionsFromFen(variant, startingFen, uciMoves)

  def passMove(variant: Variant): Int = {
    val gameSize: Int = variant.boardSize.height
    gameSize * gameSize
  }

  def uciToMove(uciMove: String, variant: Variant): Int = {
    if (uciMove == "pass" || ScalaPosition.isSelectSquares(uciMove)) passMove(variant)
    else {
      val gameSize: Int = variant.boardSize.height
      val dest          = uciMove.drop(2)

      val fileChar  = dest.charAt(0)
      val file: Int = File.fromChar(fileChar).map(_.index).getOrElse(0) // 0 index
      val rank: Int = dest.drop(1).toIntOption.getOrElse(0)             // 1 index

      gameSize * (rank - 1) + file
    }
  }

  def moveToUci(move: Int, variant: Variant): String = {
    if (move == passMove(variant)) "pass"
    else {
      val gameSize: Int = variant.boardSize.height
      val file: String  = File(move % gameSize).map(_.toString).getOrElse("a")
      val rank: Int     = (move / gameSize) + 1

      s"${Stone.forsyth.toUpper}@${file}${rank}"
    }
  }

  def moveToPos(move: Int, variant: Variant): Option[Pos] = {
    val gameSize: Int = variant.boardSize.height
    val file: String  = File(move % gameSize).map(_.toString).getOrElse("a")
    val rank: Int     = (move / gameSize) + 1
    Pos.fromKey(s"${file}${rank}")
  }

  def initialFen(variantKey: String): FEN =
    Variant(variantKey)
      .map(_.initialFen)
      .getOrElse(sys.error(s"not given a go variant name: ${variantKey}"))

  private val fenRegex =
    "([0-9Ss]?){1,19}(/([0-9Ss]?){1,19}){8,18}\\[[Ss]+\\] [w|b] (-|[a-s][1-9][0-9]?) [0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-3] [0-9]+"

  def validateFEN(@nowarn variant: Variant, fenString: String): Boolean = validateFEN(fenString)

  def validateFEN(fenString: String): Boolean =
    fenString.matches(fenRegex) && strategygames.go.engine.GoFen.parse(fenString).isRight

  def pieceMapFromFen(variantKey: String, fenString: String): PieceMap = {
    positionFromVariantNameAndFEN(variantKey, fenString).pieceMap
  }

  def writeBoardFenFromPieceMap(pieceMap: PieceMap, variant: Variant): String = {
    val gameSize: Int      = variant.boardSize.height
    val gameRow: List[Int] = List.range(0, gameSize)
    val boardString        = gameRow.reverse
      .map(y =>
        gameRow
          .map(x => {
            val piece = moveToPos(y * gameSize + x, variant).flatMap(pieceMap.get(_))
            piece.fold("1") { p => if (p.player == P1) "S" else "s" }
          })
          .mkString("")
      )
      .mkString("/")

    "[1]{2,}".r.replaceAllIn(boardString, s => s.group(0).size.toString)
  }

  def removeDeadStones(deadStones: List[Pos], fenString: String, variant: Variant): String = {
    val pieceMap        = pieceMapFromFen(variant.key, fenString)
    val updatedPieceMap = pieceMap -- deadStones.toSet
    val boardString     = writeBoardFenFromPieceMap(updatedPieceMap, variant)

    val start = fenString.indexOf("[", 0)
    if (start > 0)
      boardString + fenString.substring(start, fenString.length)
    else boardString
  }

}
