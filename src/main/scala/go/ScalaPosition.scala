package strategygames.go

import strategygames.Score
import strategygames.go.Api.Position
import strategygames.go.engine.{ GoFen, GoGame, GoState }
import strategygames.go.format.FEN
import strategygames.go.variant.Variant

/** The pure-Scala engine behind [[strategygames.go.Api.Position]], the seam every go consumer — Board,
  * Situation, Game, Forsyth, Replay — already speaks. See `docs/go-engine.md`.
  *
  * The variant is passed in rather than inferred from the FEN: board size alone identifies the
  * joansala-backed variant of that size, so a position that lost its variant would route back to the other
  * engine.
  */
final private[go] class ScalaPosition(
    game: GoGame,
    positionVariant: Variant,
    startingFen: Option[FEN],
    parentStones: Option[ScalaPosition.ParentStones] = None
) extends Position {

  private var currentGame: GoGame = game

  private var parentStonesUntilForced: Option[ScalaPosition.ParentStones] = parentStones

  lazy val variant: Variant = positionVariant

  def makeMoves(movesList: List[String]): Position =
    positionAfter(movesList.foldLeft(currentGame)(afterLegalUci), movesList)

  private[go] def makeMovesNoLegalCheck(movesList: List[String]): Position =
    positionAfter(movesList.foldLeft(currentGame)(afterUci), movesList)

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

  // NOTE: the engine state is immutable; these vars exist because `Position.setKomi` returns Unit
  // and because `pieceMap` releases its parent below.
  def setKomi(komi: Double): Unit = currentGame = currentGame.withKomi(komi)

  def deepCopy: Position =
    new ScalaPosition(currentGame, variant, startingFen, Some(inheritanceFor(Nil)))

  lazy val turn: String    = currentGame.state.turn
  lazy val playerTurn: Int = currentGame.state.playerTurn
  lazy val initialFen: FEN = startingFen.getOrElse(variant.initialFen)
  lazy val fen: FEN        = FEN(fenString)
  lazy val fenScore: Score = Score(currentGame.p1FenScore, currentGame.p2FenScore)

  /** Stones by square. One placement usually changes a handful of entries, so a position built from another
    * derives its map from the parent's instead of walking the board.
    *
    * Dropping the parent reference once the map is memoized is what keeps a replay from pinning every
    * position it ever passed through: a chain of a thousand positions would otherwise hold a thousand live
    * ancestors.
    */
  lazy val pieceMap: PieceMap = {
    val stones = parentStonesUntilForced.flatMap(stonesInheritedFrom).getOrElse(stonesByFullScan)
    parentStonesUntilForced = None
    stones
  }

  private def stonesInheritedFrom(parent: ScalaPosition.ParentStones): Option[PieceMap] =
    parent.playedUcis match {
      case Nil                                  => Some(parent.stones())
      case "pass" :: Nil                        => Some(parent.stones())
      case uci :: Nil if isSinglePlacement(uci) =>
        Pos.fromKey(uci.drop(2)).map { pos =>
          val state           = currentGame.state
          val withoutCaptured =
            state.capturedMovesOnLastPlacement.foldLeft(parent.stones()) { (stones, capturedMove) =>
              stones - ScalaPosition.posAtGridIndex(gridIndexOfMove(capturedMove, state.size))
            }
          withoutCaptured.updated(pos, Piece(moverOf(parent.stateBefore), Stone))
        }
      case _                                    => None
    }

  private def isSinglePlacement(uci: String): Boolean =
    uci.length > 2 && uci.charAt(1) == '@'

  private def moverOf(stateBefore: GoState) =
    if (stateBefore.playerTurn == GoState.BlackPlayer) P1 else P2

  private def stonesByFullScan: PieceMap = {
    val state      = currentGame.state
    val size       = state.size
    val blackStone = Piece(P1, Stone)
    val whiteStone = Piece(P2, Stone)
    val stones     = Map.newBuilder[Pos, Piece]
    var point      = 0
    while (point < state.passMove) {
      val owner = state.stoneOwnerAt(point)
      if (owner != GoState.NoOwner) {
        val pos = ScalaPosition.posAtGridIndex(gridIndexOfMove(point, size))
        stones += pos -> (if (owner == GoState.BlackPlayer) blackStone else whiteStone)
      }
      point += 1
    }
    stones.result()
  }

  private def gridIndexOfMove(move: Int, size: Int): Int =
    File.count * (move / size) + move % size

  lazy val pocketData: Option[PocketData] = Api.stonePocketData

  lazy val gameResult: GameResult = GameResult.resultFromInt(gameOutcome, gameEnd, isRepetition)
  lazy val gameEnd: Boolean       = currentGame.ended
  lazy val gameOutcome: Int       = currentGame.gameOutcome
  // NOTE: not a stub. Superko is refused at move generation, so no repeating position is reachable
  // and this engine can never observe one.
  lazy val isRepetition: Boolean  = false
  lazy val gameScore: Int         = currentGame.gameScore
  lazy val p1Score: Double        = currentGame.p1Score
  lazy val p2Score: Double        = currentGame.p2Score

  lazy val legalActions: Array[Int] =
    if (gameEnd) Array.empty else currentGame.state.legalMoves

  lazy val legalDrops: Array[Int] =
    if (gameEnd) Array.empty else currentGame.state.legalDropsShared

  def fenString: String = GoFen.render(currentGame)

  private def positionAfter(played: GoGame, playedUcis: List[String]): Position =
    new ScalaPosition(played, variant, startingFen, Some(inheritanceFor(playedUcis)))

  private def inheritanceFor(playedUcis: List[String]): ScalaPosition.ParentStones =
    ScalaPosition.ParentStones(() => pieceMap, currentGame.state, playedUcis)

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

}

private[go] object ScalaPosition {

  final private case class ParentStones(
      stones: () => PieceMap,
      stateBefore: GoState,
      playedUcis: List[String]
  )

  private val posAtGridIndex: Array[Pos] = Pos.all.toArray

  def initial(variant: Variant, komi: Double): Position =
    new ScalaPosition(GoGame.initial(variant.boardSize.height, komi), variant, None)

  def fromFen(variant: Variant, fen: FEN): Position =
    GoFen.parse(fen.value) match {
      case Right(game) => new ScalaPosition(game, variant, Some(fen))
      case Left(error) => sys.error(s"Invalid ${variant.key} fen (${error}): ${fen.value}")
    }

}
