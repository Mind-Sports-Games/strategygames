package strategygames.go

import strategygames.Score
import strategygames.go.Api.Position
import strategygames.go.engine.{ BulkReplay, GoFen, GoGame, GoState }
import strategygames.go.format.FEN
import strategygames.go.variant.Variant

/** The engine behind [[strategygames.go.Api.Position]], the seam every go consumer — Board, Situation, Game,
  * Forsyth, Replay — already speaks. See `docs/go-engine.md`.
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
  // and because `pieceMap` releases its parent below. Precondition: set the komi before reading
  // `fen`, `fenScore` or `gameResult`, which cache the komi they were first read with.
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
    * ancestors. Until this is forced, though, that chain is exactly what the position holds — around 9KB of
    * engine state per unforced ancestor, released one recursive step at a time when it finally is.
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

  private def isSinglePlacement(uci: String): Boolean = ScalaPosition.isSinglePlacement(uci)

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
  // NOTE: not a stub. Superko is refused at move generation, so no repeating position is reachable
  // and this engine can never observe one.
  lazy val isRepetition: Boolean  = false

  // NOTE: derived rather than cached, so that a `setKomi` after construction cannot leave a stale
  // score behind. The area score they all read is itself cached on the immutable state.
  def gameOutcome: Int = currentGame.gameOutcome
  def gameScore: Int   = currentGame.gameScore
  def p1Score: Double  = currentGame.p1Score
  def p2Score: Double  = currentGame.p2Score

  lazy val legalActions: Array[Int] =
    if (gameEnd) Array.empty else currentGame.state.legalMoves

  lazy val legalDrops: Array[Int] =
    if (gameEnd) Array.empty else currentGame.state.legalDrops

  def fenString: String = GoFen.render(currentGame)

  private def positionAfter(played: GoGame, playedUcis: List[String]): Position =
    new ScalaPosition(played, variant, startingFen, Some(inheritanceFor(playedUcis)))

  private def inheritanceFor(playedUcis: List[String]): ScalaPosition.ParentStones =
    ScalaPosition.ParentStones(() => pieceMap, currentGame.state, playedUcis)

  // NOTE: the first branch takes deepCopy rather than reloading from the fen — deliberate,
  // because the copy keeps the superko hash history a FEN reload cannot carry.
  private def positionBefore(previousMoves: List[String]): Position =
    if (previousMoves.isEmpty && variant.initialFen.value != fen.value) deepCopy
    else if (variant.initialFen.value != initialFen.value) ScalaPosition.fromFen(variant, initialFen)
    else ScalaPosition.initial(variant, currentGame.komi)

  private def afterLegalUci(played: GoGame, uci: String): GoGame = {
    if (played.ended)
      sys.error(s"Action ${uci} offered to a finished ${variant.key} game")
    if (isSelectSquares(uci)) played.selectDeadStones(deadStoneMoves(uci))
    else {
      val move = engineMoveOf(uci)
      if (!played.state.isLegal(move))
        sys.error(
          s"Illegal action ${uci} for ${variant.key}: legal actions ${played.state.legalMoves.mkString(", ")}"
        )
      played.play(move)
    }
  }

  private def afterUci(played: GoGame, uci: String): GoGame =
    if (isSelectSquares(uci)) played.selectDeadStones(deadStoneMoves(uci))
    else played.play(engineMoveOf(uci))

  private def isSelectSquares(uci: String): Boolean = ScalaPosition.isSelectSquares(uci)

  private def engineMoveOf(uci: String): Int = ScalaPosition.engineMoveOf(uci, variant)

  private def deadStoneMoves(uci: String): List[Int] = ScalaPosition.deadStonePoints(uci, variant)

}

private[go] object ScalaPosition {

  final private case class ParentStones(
      stones: () => PieceMap,
      stateBefore: GoState,
      playedUcis: List[String]
  )

  final private class SeamPlan(
      val engineMoves: Array[Int],
      val trailingDeadStones: Option[List[Int]]
  )

  private val PassAction      = "pass"
  private val DeadStonePrefix = "ss:"

  private val posAtGridIndex: Array[Pos] = Pos.all.toArray

  def initial(variant: Variant, komi: Double): Position =
    new ScalaPosition(GoGame.initial(variant.boardSize.height, komi), variant, None)

  def fromFen(variant: Variant, fen: FEN): Position =
    new ScalaPosition(parsedGame(variant, fen), variant, Some(fen))

  private[go] def batchFromFen(variant: Variant, startingFen: FEN, ucis: List[String]): Position =
    batchFrom(resumedGame(variant, startingFen), variant, Some(startingFen), ucis)

  private[go] def batchFromInitial(variant: Variant, ucis: List[String]): Position =
    batchFrom(GoGame.initial(variant.boardSize.height, variant.komi), variant, None, ucis)

  private[go] def positionsFromFen(
      variant: Variant,
      startingFen: FEN,
      ucis: List[String]
  ): Vector[Position] = {
    val start     = resumedGame(variant, startingFen)
    val actions   = ucis.toArray
    val plan      = planOf(variant, actions, start.ended)
    val positions = Vector.newBuilder[Position]
    positions.sizeHint(actions.length + 1)

    var game               = start
    var position: Position = new ScalaPosition(start, variant, Some(startingFen))
    positions += position

    def advanceTo(played: GoGame, uci: String): Unit = {
      val parent    = position
      val inherited = ParentStones(() => parent.pieceMap, game.state, List(uci))
      game = played
      position = new ScalaPosition(played, variant, Some(startingFen), Some(inherited))
      positions += position
    }

    var index = 0
    while (index < plan.engineMoves.length) {
      val move = plan.engineMoves(index)
      if (!game.state.isLegal(move))
        sys.error(
          s"Illegal action ${actions(index)} at ply ${index} for ${variant.key}: " +
            s"legal actions ${game.state.legalMoves.mkString(", ")}"
        )
      advanceTo(game.play(move), actions(index))
      index += 1
    }
    plan.trailingDeadStones.foreach(deadStones =>
      advanceTo(game.selectDeadStones(deadStones), actions(index))
    )
    positions.result()
  }

  private def batchFrom(
      start: GoGame,
      variant: Variant,
      startingFen: Option[FEN],
      ucis: List[String]
  ): Position = {
    val actions = ucis.toArray
    val plan    = planOf(variant, actions, start.ended)
    val folded  = foldedThrough(variant, start, actions, plan)
    new ScalaPosition(plan.trailingDeadStones.fold(folded)(folded.selectDeadStones), variant, startingFen)
  }

  private def foldedThrough(
      variant: Variant,
      start: GoGame,
      actions: Array[String],
      plan: SeamPlan
  ): GoGame =
    try
      GoGame(
        BulkReplay.replay(start.state, plan.engineMoves),
        start.komi,
        start.plyCount + plan.engineMoves.length,
        start.deadStonesSelected
      )
    catch {
      case illegal: BulkReplay.IllegalMoveAt =>
        throw new RuntimeException(
          s"Illegal action ${actions(illegal.index)} at ply ${illegal.index} for ${variant.key}: " +
            s"legal actions ${illegal.legalMoves.mkString(", ")}",
          illegal
        )
    }

  private def planOf(variant: Variant, actions: Array[String], startEnded: Boolean): SeamPlan = {
    val engineMoves     = new Array[Int](actions.length)
    var engineMoveCount = 0
    var deadStones      = Option.empty[List[Int]]
    var ply             = 0
    while (ply < actions.length) {
      val uci = actions(ply)
      if (startEnded || deadStones.isDefined)
        sys.error(s"Action ${uci} at ply ${ply} offered to a finished ${variant.key} game")
      if (isSelectSquares(uci)) deadStones = Some(deadStonePoints(uci, variant))
      else {
        engineMoves(engineMoveCount) = plannedEngineMove(uci, variant, ply)
        engineMoveCount += 1
      }
      ply += 1
    }
    new SeamPlan(java.util.Arrays.copyOf(engineMoves, engineMoveCount), deadStones)
  }

  private def plannedEngineMove(uci: String, variant: Variant, ply: Int): Int =
    if (uci == PassAction) Api.passMove(variant)
    else if (isSinglePlacement(uci))
      dropPoint(uci, variant).getOrElse(
        sys.error(s"Drop ${uci} at ply ${ply} names no square of ${variant.key}")
      )
    else sys.error(s"Unreadable action ${uci} at ply ${ply} for ${variant.key}")

  private[go] def isSelectSquares(uci: String): Boolean = uci.take(3) == DeadStonePrefix

  private def isSinglePlacement(uci: String): Boolean =
    uci.length > 2 && uci.charAt(1) == '@' && Role.allByForsyth.contains(uci.charAt(0).toLower)

  /** `Api.uciToMove` folds anything that is not `pass` onto a point of the board, so a drop whose key names
    * no square of this variant — or a token of some other shape entirely — would silently be played somewhere
    * else. Only `pass` and an on-board placement get through to it.
    */
  private def engineMoveOf(uci: String, variant: Variant): Int =
    if (uci == PassAction) Api.uciToMove(uci, variant)
    else if (isSinglePlacement(uci)) {
      if (dropPoint(uci, variant).isEmpty)
        sys.error(s"Drop ${uci} names no square of ${variant.key}")
      Api.uciToMove(uci, variant)
    } else sys.error(s"Unreadable action ${uci} for ${variant.key}")

  private def dropPoint(uci: String, variant: Variant): Option[Int] =
    Pos.fromKey(uci.drop(2)).filter(isOnBoard(_, variant)).map(pointOf(_, variant))

  /** A key naming a square the board does not have is ignored: dead stone selection comes from a client, and
    * a square with no stone on it lifts nothing either way. A key that is not a coordinate at all is a caller
    * bug and says so.
    */
  private def deadStonePoints(uci: String, variant: Variant): List[Int] =
    uci.drop(3).split(",").toList.filter(_.nonEmpty).flatMap { key =>
      Pos.fromKey(key) match {
        case Some(pos) => Option.when(isOnBoard(pos, variant))(pointOf(pos, variant))
        case None      => sys.error(s"Unreadable dead stone ${key} in ${uci}")
      }
    }

  private def isOnBoard(pos: Pos, variant: Variant): Boolean =
    pos.file.index < variant.boardSize.width && pos.rank.index < variant.boardSize.height

  private def pointOf(pos: Pos, variant: Variant): Int =
    variant.boardSize.height * pos.rank.index + pos.file.index

  private def resumedGame(variant: Variant, startingFen: FEN): GoGame = {
    if (variant.boardSize.height != startingFen.gameSize)
      sys.error(s"incorrect variant name (${variant.key}) and/or fen (${startingFen})")
    parsedGame(variant, startingFen)
  }

  private def parsedGame(variant: Variant, fen: FEN): GoGame =
    GoFen.parse(fen.value) match {
      case Right(game) => game
      case Left(error) => sys.error(s"Invalid ${variant.key} fen (${error}): ${fen.value}")
    }

}
