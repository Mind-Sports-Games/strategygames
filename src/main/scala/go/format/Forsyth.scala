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

  val initial = FEN(
    "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19[SSSSSSSSSSssssssssss] b - 0 75 0 0 75 0 1"
  )

  def <<@(variant: Variant, fen: FEN): Option[Situation] =
    Some(
      Situation(
        Board(
          pieces = fen.pieces,
          history = History(
            captures = Score(fen.player1Captures, fen.player2Captures),
            halfMoveClock = fen.ply.getOrElse(sys.error(s"go fen states no move number: ${fen.value}")).max(0)
          ),
          variant = variant,
          pocketData = Api.stonePocketData,
          komi = fen.komi,
          ko = fen.ko,
          consecutivePasses = fen.fenPassCount match {
            case 1 => 1
            case 2 => 2
            case _ => 0
          },
          deadStonesSelected = fen.fenPassCount == settledPassCount
        ).withHistoryStartingHere,
        fen.value.split(' ')(1) match {
          case "b" => P1
          case "w" => P2
          case _   => sys.error("Invalid player in fen")
        }
      )
    )

  def <<(fen: FEN): Option[Situation] = <<@(fen.variant, fen)

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

  def boardPart(board: Board): String =
    ranksTopDown(board).map(renderedRank(board, _)).mkString("", "/", pocket)

  private def ranksTopDown(board: Board): List[Int] =
    (board.variant.boardSize.height - 1 to 0 by -1).toList

  private def renderedRank(board: Board, rankIndex: Int): String = {
    val (rendered, trailingEmpties) =
      (0 until board.variant.boardSize.width).foldLeft((List.empty[String], 0)) {
        case ((rendered, emptyRun), fileIndex) =>
          Pos.at(fileIndex, rankIndex).flatMap(board.pieces.get) match {
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
    val fullMove                     = board.history.halfMoveClock / 2 + 1
    val settledByP2                  = board.history.lastTurn.headOption.exists {
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
