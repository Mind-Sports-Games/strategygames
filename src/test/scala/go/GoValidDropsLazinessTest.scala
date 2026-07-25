package strategygames.go

import org.specs2.mutable.Specification

import strategygames.Score
import strategygames.go.format.FEN
import strategygames.go.variant.{ Go19x19, Go9x9, Variant }

final private class EngineApplyCountingPosition(delegate: Api.Position) extends Api.Position {

  private var applies: Int = 0

  def enginePlacementsApplied: Int = applies

  lazy val variant: Variant = delegate.variant

  def makeMoves(movesList: List[String]): Api.Position = delegate.makeMoves(movesList)

  private[go] def makeMovesNoLegalCheck(movesList: List[String]): Api.Position =
    delegate.makeMovesNoLegalCheck(movesList)

  def makeMovesWithPrevious(movesList: List[String], previousMoves: List[String]): Api.Position =
    delegate.makeMovesWithPrevious(movesList, previousMoves)

  def createPosFromPrevious(previousMoves: List[String]): Api.Position =
    delegate.createPosFromPrevious(previousMoves)

  private[go] def makeMovesWithPosUnchecked(
      movesList: List[String],
      posWithPrevious: Api.Position
  ): Api.Position = {
    applies += 1
    delegate.makeMovesWithPosUnchecked(movesList, posWithPrevious)
  }

  def setKomi(komi: Double): Unit = delegate.setKomi(komi)

  def deepCopy: Api.Position = delegate.deepCopy

  lazy val turn: String                   = delegate.turn
  lazy val initialFen: FEN                = delegate.initialFen
  lazy val fen: FEN                       = delegate.fen
  lazy val pieceMap: PieceMap             = delegate.pieceMap
  lazy val pocketData: Option[PocketData] = delegate.pocketData
  lazy val fenScore: Score                = delegate.fenScore
  lazy val gameResult: GameResult         = delegate.gameResult
  lazy val gameEnd: Boolean               = delegate.gameEnd
  lazy val isRepetition: Boolean          = delegate.isRepetition
  lazy val legalDrops: Array[Int]         = delegate.legalDrops
  lazy val legalActions: Array[Int]       = delegate.legalActions
  lazy val playerTurn: Int                = delegate.playerTurn

  def gameOutcome: Int  = delegate.gameOutcome
  def gameScore: Int    = delegate.gameScore
  def p1Score: Double   = delegate.p1Score
  def p2Score: Double   = delegate.p2Score
  def fenString: String = delegate.fenString
}

class GoValidDropsLazinessTest extends Specification {

  private def eagerlyAppliedValidDrops(situation: Situation): List[Drop] =
    situation.board.apiPosition.legalDrops
      .map(dest => (dest, Api.moveToPos(dest, situation.board.variant)))
      .flatMap {
        case (_, Some(dest)) =>
          val nextBoard = situation.board.afterDrop(situation.player, dest)
          Option.unless(nextBoard.apiPosition.isRepetition)(
            Drop(
              piece = Piece(situation.player, Role.defaultRole),
              pos = dest,
              situationBefore = situation,
              nextBoard = LazyBoardAfter(() => nextBoard),
              autoEndTurn = true
            )
          )
        case (destInt, dest) => sys.error(s"Invalid pos from int: ${destInt}, ${dest}")
      }
      .toList

  private def situationAfter(variant: Variant, actionKeys: List[String]): Situation =
    actionKeys
      .foldLeft(Game(variant)) { (game, key) =>
        if (key == "pass") game.apply(variant.validPass(game.situation))
        else game.apply(variant.validDrops(game.situation).filter(_.pos.key == key).head)
      }
      .situation

  private val emptyBoard9x9   = situationAfter(Go9x9, Nil)
  private val emptyBoard19x19 = situationAfter(Go19x19, Nil)

  private val midGame9x9 =
    situationAfter(Go9x9, List("e5", "e6", "d5", "d6", "f5", "f6", "e4", "c5", "c4"))

  private val midGame19x19 = situationAfter(
    Go19x19,
    List("d4", "q16", "q4", "d16", "f17", "c6", "r14", "f3", "d10", "k10")
  )

  private val superkoRefused = situationAfter(
    Go9x9,
    List("c6", "c5", "c7", "d6", "e6", "e5", "e7", "d4", "d8", "e4", "pass", "d7", "d5")
  )

  private val positionsUnderTest = List(
    ("an empty 9x9 board", emptyBoard9x9),
    ("an empty 19x19 board", emptyBoard19x19),
    ("a 9x9 mid-game position", midGame9x9),
    ("a 19x19 mid-game position", midGame19x19),
    ("the 9x9 position whose superko recapture is refused", superkoRefused)
  )

  private def boardAfterFields(drop: Drop) = {
    val board = drop.after
    (
      board.pieces,
      board.uciMoves,
      board.pocketData,
      board.apiPosition.fen,
      board.history.captures,
      board.history.halfMoveClock,
      board.history.score
    )
  }

  private def sampledIndices(size: Int): List[Int] = List(0, size / 2, size - 1).distinct

  private def countingSituationOf(situation: Situation) = {
    val counting = new EngineApplyCountingPosition(situation.board.apiPosition)
    (counting, situation.copy(board = situation.board.copy(position = Some(counting))))
  }

  "validDrops" should {

    positionsUnderTest.foreach { case (description, situation) =>
      val variant  = situation.board.variant
      val produced = variant.validDrops(situation)
      val oracle   = eagerlyAppliedValidDrops(situation)

      s"offer the same drops as an eagerly applied reference on ${description}" in {
        produced.map(_.pos) === oracle.map(_.pos)
        produced.map(_.piece) === oracle.map(_.piece)
        produced.map(_.autoEndTurn) === oracle.map(_.autoEndTurn)
        produced.size === situation.board.apiPosition.legalDrops.length
      }

      s"reach the same board after a forced drop as the reference on ${description}" in {
        sampledIndices(produced.size).map(index => boardAfterFields(produced(index))) ===
          sampledIndices(oracle.size).map(index => boardAfterFields(oracle(index)))
      }
    }

    "apply no placement to the engine until a drop is forced" in {
      val (counting, situation) = countingSituationOf(midGame19x19)
      val drops                 = situation.board.variant.validDrops(situation)

      drops.size must be_>(300)
      counting.enginePlacementsApplied === 0
    }

    "apply one placement to the engine for each drop that is forced" in {
      val (counting, situation) = countingSituationOf(midGame19x19)
      val drops                 = situation.board.variant.validDrops(situation)

      drops.head.after.pieces.size === situation.board.pieces.size + 1
      counting.enginePlacementsApplied === 1

      drops(1).after.pieces.size === situation.board.pieces.size + 1
      counting.enginePlacementsApplied === 2
    }

    "count one engine placement per legal point when applied eagerly" in {
      val (counting, situation) = countingSituationOf(midGame19x19)
      val drops                 = eagerlyAppliedValidDrops(situation)

      counting.enginePlacementsApplied === drops.size
    }
  }
}
