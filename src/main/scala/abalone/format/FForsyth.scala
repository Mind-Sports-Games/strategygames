package abalone.format

import abalone.{BBoard, GGame, HHistory, SSituation}
import abalone.util.geometry.Cell
import strategygames.abalone._
import strategygames.abalone.format.FEN
import strategygames.abalone.variant.Variant
import strategygames.{Player, Score}

/** Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  */
object FForsyth {
  def <<@(variant: Variant, fen: FEN): Option[SSituation] = {
    Some(
      SSituation(
        BBoard(
          pieces = fen.pieces(variant.boardType),
          history = HHistory(),
          variant = variant
        ),
        fen.value.split(' ')(3) match {
          case "b" => P1
          case "w" => P2
          case _ => sys.error("Invalid player in fen")
        }
      ).withHistory(
        HHistory(
          score = Score(fen.player1Score, fen.player2Score),
          halfMoveClock = fen.halfMovesSinceLastCapture.getOrElse(0)
        )
      )
    )
  }

  def <<(fen: FEN): Option[SSituation] = <<@(Variant.default, fen)

  case class SituationPlus(situation: SSituation, fullTurnCount: Int) {
    def turnCount = fullTurnCount * 2 - situation.player.fold(2, 1)

    // when we get a multiaction variant we should set this
    def plies = turnCount
  }

  def <<<@(variant: Variant, fen: FEN): Option[SituationPlus] =
    <<@(variant, fen) map { sit =>
      SituationPlus(
        // not doing half move clock history like we do in chess
        sit,
        fen.value.split(' ').last.toIntOption.map(_ max 1 min 500) | 1
      )
    }

  def <<<(fen: FEN): Option[SituationPlus] = <<<@(Variant.default, fen)

  def >>(situation: SSituation): FEN = >>(SituationPlus(situation, 1))

  def >>(parsed: SituationPlus): FEN =
    parsed match {
      case SituationPlus(situation, _) =>
        >>(GGame(situation, plies = parsed.plies, turnCount = parsed.turnCount))
    }

  def >>(game: GGame): FEN = {
    val boardFen = getFen_board(game.situation.board)
    val scoreStr = game.situation.board.history.score.fenStr
    val player = game.situation.player.fold('b', 'w')
    val halfMoves = game.situation.board.history.halfMoveClock
    val fullMoves = game.fullTurnCount
    FEN(s"${boardFen} ${scoreStr} ${player} ${halfMoves} ${fullMoves}")
  }

  def exportBoard(board: BBoard): String = {
    val boardFen = getFen_board(board)
    val scoreStr = board.history.score.fenStr
    s"${boardFen} ${scoreStr}"
  }

  def getFen_board(board: BBoard): String = {
    val res = new StringBuilder(board.variant.boardType.cellList.size)

    var prev: Option[Cell] = Option.empty
    var emptyNb = 0

    def writeEmptyNb = if (emptyNb > 0) {
      res.append(s"$emptyNb")
      emptyNb = 0
    }

    board.variant.boardType.cellList.foreach(a => {
      if (prev.isDefined & prev.get.y != a.y) {
        writeEmptyNb
        res.append("/")
      }

      board.getPiece(a) match {
        case None => emptyNb += 1
        case Some(piece) =>
          writeEmptyNb
          res.append(if (piece.player == P1) piece.forsyth.toString.toUpperCase() else piece.forsyth.toString.toLowerCase())
      }

      prev = Option(a);
    })

    res.toString
  }

  def boardAndPlayer(situation: SSituation): String = boardAndPlayer(situation.board, situation.player)

  def boardAndPlayer(board: BBoard, turnPlayer: Player): String = s"${exportBoard(board)} ${turnPlayer.letter}"
}