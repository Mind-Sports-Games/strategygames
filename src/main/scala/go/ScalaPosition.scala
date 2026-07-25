package strategygames.go

import strategygames.Player
import strategygames.go.Api.Position
import strategygames.go.engine.{ GoFen, GoGame, GoState }
import strategygames.go.format.FEN
import strategygames.go.variant.Variant

final private[go] class ScalaPosition(
    game: GoGame,
    positionVariant: Variant,
    startingFen: Option[FEN]
) extends Position {

  private var currentGame: GoGame = game

  lazy val variant: Variant = positionVariant

  def makeMoves(movesList: List[String]): Position =
    positionAfter(movesList.foldLeft(currentGame)(afterLegalUci))

  private[go] def makeMovesNoLegalCheck(movesList: List[String]): Position =
    positionAfter(movesList.foldLeft(currentGame)(afterUci))

  def makeMovesWithPrevious(movesList: List[String], previousMoves: List[String]): Position =
    createPosFromPrevious(previousMoves).makeMoves(movesList)

  def createPosFromPrevious(previousMoves: List[String]): Position =
    positionBefore(previousMoves).makeMoves(previousMoves)

  private[go] def makeMovesWithPosUnchecked(
      movesList: List[String],
      posWithPrevious: Position
  ): Position = {
    val position = posWithPrevious.makeMovesNoLegalCheck(movesList)
    position.setKomi(currentGame.komi)
    position
  }

  def setKomi(komi: Double): Unit = currentGame = currentGame.withKomi(komi)

  def deepCopy: Position = new ScalaPosition(currentGame, variant, startingFen)

  lazy val turn: String       = currentGame.state.turn
  lazy val playerTurn: Int    = currentGame.state.playerTurn
  lazy val initialFen: FEN    = startingFen.getOrElse(variant.initialFen)
  lazy val fen: FEN           = FEN(fenString)
  lazy val pieceMap: PieceMap = stonePlacements.toMap

  lazy val pocketData: Option[PocketData] = Api.stonePocketData

  lazy val gameResult: GameResult = GameResult.resultFromInt(gameOutcome, gameEnd, isRepetition)
  lazy val gameEnd: Boolean       = currentGame.ended
  lazy val gameOutcome: Int       = currentGame.gameOutcome
  lazy val isRepetition: Boolean  = false
  lazy val gameScore: Int         = currentGame.gameScore
  lazy val p1Score: Double        = currentGame.p1Score
  lazy val p2Score: Double        = currentGame.p2Score

  lazy val legalActions: Array[Int] =
    if (gameEnd) Array.empty else currentGame.state.legalMoves

  lazy val legalDrops: Array[Int] = legalActions.filter(_ != Api.passMove(variant))

  def fenString: String = GoFen.render(currentGame)

  private def positionAfter(played: GoGame): Position =
    new ScalaPosition(played, variant, startingFen)

  private def positionBefore(previousMoves: List[String]): Position =
    if (previousMoves.isEmpty && variant.initialFen.value != fen.value) deepCopy
    else if (variant.initialFen.value != initialFen.value) ScalaPosition.fromFen(variant, initialFen)
    else ScalaPosition.initial(variant, currentGame.komi)

  private def afterLegalUci(played: GoGame, uci: String): GoGame = {
    if (!isSelectSquares(uci) && !played.state.isLegal(Api.uciToMove(uci, variant)))
      sys.error(
        s"Illegal action ${uci} for ${variant.key}: legal actions ${played.state.legalMoves.mkString(", ")}"
      )
    afterUci(played, uci)
  }

  private def afterUci(played: GoGame, uci: String): GoGame =
    if (isSelectSquares(uci)) played.selectDeadStones(deadStoneMoves(uci))
    else played.play(Api.uciToMove(uci, variant))

  private def isSelectSquares(uci: String): Boolean = uci.take(3) == "ss:"

  private def deadStoneMoves(uci: String): List[Int] =
    uci.drop(3).split(",").toList.flatMap(Pos.fromKey(_)).map(pointOf)

  private def pointOf(pos: Pos): Int = variant.boardSize.height * pos.rank.index + pos.file.index

  private def stonePlacements: Iterable[(Pos, Piece)] =
    (0 until Api.passMove(variant)).flatMap { point =>
      playerOfStone(currentGame.state.stoneOwnerAt(point)).flatMap { player =>
        Api.moveToPos(point, variant).map(_ -> Piece(player, Stone))
      }
    }

  private def playerOfStone(stoneOwner: Int): Option[Player] =
    if (stoneOwner == GoState.BlackPlayer) Some(P1)
    else if (stoneOwner == GoState.WhitePlayer) Some(P2)
    else None

}

private[go] object ScalaPosition {

  def initial(variant: Variant, komi: Double): Position =
    new ScalaPosition(GoGame.initial(variant.boardSize.height, komi), variant, None)

  def fromFen(variant: Variant, fen: FEN): Position =
    GoFen.parse(fen.value) match {
      case Right(game) => new ScalaPosition(game, variant, Some(fen))
      case Left(error) => sys.error(s"Invalid ${variant.key} fen (${error}): ${fen.value}")
    }

}
