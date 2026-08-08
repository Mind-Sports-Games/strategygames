package strategygames.go.format

import cats.implicits._
import scalalib.extensions.*

import strategygames.Score
import strategygames.Player
import strategygames.go._
import strategygames.go.variant.Variant

/** Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  */
object Forsyth {

  private val settledPassCount          = 3
  private val highestPassCount          = 2
  private val pocket                    = "[SSSSSSSSSSssssssssss]"
  private val noKoPoint                 = "-"
  private val fenTenths                 = 10
  private val digitAppendedBySettlement = 1
  private val firstCountedField         = 3
  private val fieldCountsOfTheGrammar   = Set(9, 10)
  private val decimalBase               = 10

  private val playerByTurnSymbol = Map("b" -> P1, "w" -> P2)

  private val tenFieldShape =
    "([0-9Ss]?){1,19}(/([0-9Ss]?){1,19}){8,18}\\[[Ss]+\\] [w|b] (-|[a-s][1-9][0-9]?)" +
      " [0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-3] [0-9]+"

  val initial = FEN(
    "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19[SSSSSSSSSSssssssssss] b - 0 75 0 0 75 0 1"
  )

  def <<@(variant: Variant, fen: FEN): Option[Situation] =
    for {
      player <- playerNamedByTurnField(fen)
      if describes(variant.boardSize, fen)
    } yield Situation(
      Board(
        pieces = fen.pieces,
        history = History(
          captures = Score(fen.player1Captures, fen.player2Captures),
          halfMoveClock = fen.ply.getOrElse(0).max(0)
        ),
        variant = variant,
        pocketData = Some(PocketData.init),
        komi = fen.komi,
        ko = fen.ko,
        consecutivePasses = fen.fenPassCount match {
          case 1 => 1
          case 2 => 2
          case _ => 0
        },
        deadStonesSelected = fen.fenPassCount == settledPassCount
      ).withHistoryStartingHere,
      player
    )

  def <<(fen: FEN): Option[Situation] = <<@(fen.variant, fen)

  def validate(fen: FEN): Boolean =
    fen.value.matches(tenFieldShape) &&
      Board.BoardSize.all.exists(size => size.height == fen.gameSize && describes(size, fen))

  private def describes(size: Board.BoardSize, fen: FEN): Boolean = {
    val fields = fen.value.split(' ').toList
    fen.gameSize == size.height &&
    fieldCountsOfTheGrammar(fields.length) &&
    fields.drop(firstCountedField).forall(_.toIntOption.isDefined) &&
    playerNamedByTurnField(fen).isDefined &&
    fen.board.split('/').forall(rowFills(size.width)) &&
    koFieldNamesAPointOf(size, fields)
  }

  private def playerNamedByTurnField(fen: FEN): Option[Player] =
    fen.value.split(' ').lift(FEN.playerIndex).flatMap(playerByTurnSymbol.get)

  private def rowFills(width: Int)(row: String): Boolean = renderedWidthOf(row).contains(width)

  private def renderedWidthOf(row: String): Option[Int] =
    row
      .foldLeft(Option((0, 0))) {
        case (Some((stones, emptyRun)), symbol) if symbol.isDigit                             =>
          Some((stones, emptyRun * decimalBase + symbol.asDigit))
        case (Some((stones, emptyRun)), symbol) if Role.allByForsyth.contains(symbol.toLower) =>
          Some((stones + emptyRun + 1, 0))
        case _                                                                                => None
      }
      .map { case (stones, trailingEmpties) => stones + trailingEmpties }

  private def koFieldNamesAPointOf(size: Board.BoardSize, fields: List[String]): Boolean =
    fields.lift(FEN.koIndex).exists { field =>
      field == noKoPoint || Pos.fromKey(field).exists(size.onBoard)
    }

  case class SituationPlus(situation: Situation, fullTurnCount: Int) {

    def turnCount = fullTurnCount * 2 - situation.player.fold(2, 1)
    // when we get a multiaction variant we should set this
    def plies     = turnCount

  }

  def <<<@(variant: Variant, fen: FEN): Option[SituationPlus] =
    <<@(variant, fen) map { sit =>
      SituationPlus(
        sit,
        fen.value.split(' ').last.toIntOption.map(_ max 1 min 500) | 1
      )
    }

  def <<<(fen: FEN): Option[SituationPlus] = <<<@(fen.variant, fen)

  def >>(situation: Situation): FEN = >>(SituationPlus(situation, 1))

  def >>(parsed: SituationPlus): FEN =
    parsed match {
      case SituationPlus(situation, _) =>
        >>(Game(situation, plies = parsed.plies, turnCount = parsed.turnCount))
    }

  def >>(game: Game): FEN = exportBoardFen(game.situation.board)

  // TODO Should this just be returning the board part of the fen? Check what Chess does
  def exportBoard(board: Board): String = exportBoardFen(board).value

  def exportBoardFen(board: Board): FEN = {
    val score = board.variant.areaScore(board)
    FEN(
      List(
        boardPart(board),
        playerToMove(board).fold("b", "w"),
        board.ko.fold(noKoPoint)(_.key),
        score.p1.toString,
        score.p2.toString,
        board.history.captures.p1.toString,
        board.history.captures.p2.toString,
        komiTenths(board).toString,
        passCount(board).toString,
        fullMovePart(board)
      ).mkString(" ")
    )
  }

  def boardPart(board: Board): String = s"${boardRows(board.variant, board.pieces)}${pocket}"

  def boardRows(variant: Variant, pieces: PieceMap): String =
    ranksTopDown(variant).map(renderedRank(variant, pieces, _)).mkString("/")

  def removeDeadStones(variant: Variant, fen: FEN, squares: List[Pos]): FEN = {
    val rows          = boardRows(variant, fen.pieces -- squares.toSet)
    val pocketOnwards = fen.value.indexOf('[')
    FEN(if (pocketOnwards > 0) rows + fen.value.substring(pocketOnwards) else rows)
  }

  private def ranksTopDown(variant: Variant): List[Int] =
    (variant.boardSize.height - 1 to 0 by -1).toList

  private def renderedRank(variant: Variant, pieces: PieceMap, rankIndex: Int): String = {
    val (rendered, trailingEmpties) =
      (0 until variant.boardSize.width).foldLeft((List.empty[String], 0)) {
        case ((rendered, emptyRun), fileIndex) =>
          Pos.at(fileIndex, rankIndex).flatMap(pieces.get) match {
            case Some(stone) => (symbolOf(stone) :: emptiesBefore(rendered, emptyRun), 0)
            case None        => (rendered, emptyRun + 1)
          }
      }
    emptiesBefore(rendered, trailingEmpties).reverse.mkString
  }

  private def emptiesBefore(rendered: List[String], emptyRun: Int): List[String] =
    if (emptyRun > 0) emptyRun.toString :: rendered else rendered

  private def symbolOf(stone: Piece): String =
    stone.player.fold(stone.forsyth.toUpper, stone.forsyth).toString

  private def komiTenths(board: Board): Int = Math.round(board.komi * fenTenths).toInt

  private def passCount(board: Board): Int =
    if (board.deadStonesSelected) settledPassCount
    else board.consecutivePasses min highestPassCount

  private def playerToMove(board: Board): Player =
    Player.fromTurnCount(board.history.halfMoveClock)

  private def fullMovePart(board: Board): String = {
    val fullMove    = board.history.halfMoveClock / 2 + 1
    val settledByP2 = board.history.lastTurn.headOption.exists {
      case _: Uci.SelectSquares => playerToMove(board).p1
      case _                    => false
    }
    if (settledByP2) s"${fullMove}${digitAppendedBySettlement}"
    else fullMove.toString
  }

  def boardAndPlayer(situation: Situation): String =
    boardAndPlayer(situation.board, situation.player)

  // TODO review this, not sure this is correct as will return full fen appended with w/b
  def boardAndPlayer(board: Board, turnPlayer: Player): String =
    s"${exportBoard(board)} ${turnPlayer.letter}"
}
