package strategygames.abalone.format

import strategygames.abalone._
import strategygames.abalone.variant.Variant
import strategygames.{Player, Score}

/** Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  */
object Forsyth {
  val initial = FEN("SS1ss/SSSsss/1SS1ss1/8/9/8/1ss1SS1/sssSSS/ss1SS 0 0 b 0 1") //TODO should NOT be here (I suppose)

  def <<@(variant: Variant, fen: FEN): Option[Situation] = {
    Some(
      Situation(
        Board(
          pieces = fen.pieces(variant.boardType),
          history = History(),
          variant = variant
        ),
        fen.value.split(' ')(3) match {
          case "b" => P1
          case "w" => P2
          case _   => sys.error("Invalid player in fen")
        }
      ).withHistory(
        History(
          score = Score(fen.player1Score, fen.player2Score),
          halfMoveClock = fen.halfMovesSinceLastCapture.getOrElse(0)
        )
      )
    )
  }

  def <<(fen: FEN): Option[Situation] = <<@(Variant.default, fen)

  case class SituationPlus(sit: Situation, fullTurnCount: Int) {
    def turnCount = fullTurnCount * 2 - sit.player.fold(2, 1)//TODO Grand Abalone

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

  def >>(sit: Situation): FEN = >>(SituationPlus(sit, 1))

  def >>(parsed: SituationPlus): FEN =
    parsed match {
      case SituationPlus(sit, _) =>
        >>(Game(sit, plies = parsed.plies, turnCount = parsed.turnCount))
    }

  def >>(game: Game): FEN = {
    val boardFen = getFen_board(game.situation.board)
    val scoreStr = game.situation.board.history.score.fenStr
    val player = game.situation.player.fold('b', 'w')
    val halfMoves = game.situation.board.history.halfMoveClock
    val fullMoves = game.fullTurnCount
    FEN(s"${boardFen} ${scoreStr} ${player} ${halfMoves} ${fullMoves}")
  }

  def exportBoard(board: Board): String = {
    val boardFen = getFen_board(board)
    val scoreStr = board.history.score.fenStr
    s"${boardFen} ${scoreStr}"
  }

  def getFen_board(board: Board): String = {
    val res = new StringBuilder(board.variant.boardType.cellList.size)

    var prev: Option[Pos] = Option.empty
    var emptyNb = 0

    def writeEmptyNb() = if (emptyNb > 0) {
      res.append(s"$emptyNb")
      emptyNb = 0
    }

    board.variant.boardType.cellList.foreach(a => {
      if (prev.isDefined & prev.get.y != a.y) {
        writeEmptyNb()
        res.append("/")
      }

      board.getPiece(a) match {
        case None => emptyNb += 1
        case Some(piece) =>
          writeEmptyNb()
          res.append(if (piece.player == P1) piece.forsyth.toString.toUpperCase() else piece.forsyth.toString.toLowerCase())
      }

      prev = Option(a);
    })

    res.toString
  }

  def boardAndPlayer(sit: Situation): String = boardAndPlayer(sit.board, sit.player)

  def boardAndPlayer(board: Board, nextPlayer: Player): String = s"${exportBoard(board)} ${nextPlayer.letter}"
}