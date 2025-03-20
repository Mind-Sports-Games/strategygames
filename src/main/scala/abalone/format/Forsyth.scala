package strategygames.abalone.format

import strategygames.abalone._
import strategygames.abalone.variant.{Abalone, Variant}
import strategygames.{Player, Score}

/** Transforms a game to standard Forsyth-Edwards notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  */
object Forsyth {
  val initial = Abalone.initialFen // TODO?

  def <<@(variant: Variant, fen: FEN): Option[Situation] = {
    val pp = variant.hasPrevPlayer
    Option(
      Situation(
        board = Board(
          pieces = fen.pieces(variant.boardType),
          history = History(
            prevPlayer = if (pp) getPlayerFromStr(fen.value.split(' ')(3)) else None,
            score = Score(fen.player1Score, fen.player2Score),
            halfMoveClock = fen.halfMovesSinceLastCapture(variant).getOrElse(0)
          ),
          variant = variant
        ),
        player = getPlayerFromStr(fen.value.split(' ')(if (pp) 4 else 3)).get
      )
    )
  }

  private def getPlayerFromStr(player: String, allowNone: Boolean = false): Option[Player] =
    player match {
      case "b" => Option(P1)
      case "w" => Option(P2)
      case _   => if (allowNone) None else sys.error("Invalid player in fen")
    }

  def <<(fen: FEN): Option[Situation] = <<@(Variant.default, fen)

  case class SituationPlus(sit: Situation, fullTurnCount: Int) {
    def turnCount = sit.board.variant.turnCountFromFen(fullTurnCount, sit.player)

    def plies = sit.board.variant.pliesFromFen(
      fullTurnCount,
      sit.player,
      sit.board.history.currentTurn.size
    )
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
    val boardFen   = getFen_board(game.situation.board)
    val scoreStr   = game.situation.board.history.score.fenStr
    val prevPlayer = if (game.situation.board.variant.hasPrevPlayer) {
      game.situation.board.history.prevPlayer.fold(" *")(p => p.fold(" b", " w"))
    } else ""
    val player     = game.situation.player.fold('b', 'w')
    val halfMoves  = game.situation.board.history.halfMoveClock
    val fullMoves  = game.fullTurnCount
    FEN(s"$boardFen $scoreStr$prevPlayer $player $halfMoves $fullMoves")
  }

  def exportBoard(board: Board): String = {
    val boardFen = getFen_board(board)
    val scoreStr = board.history.score.fenStr
    s"${boardFen} ${scoreStr}"
  }

  def getFen_board(board: Board): String = {
    val res = new StringBuilder(board.variant.boardType.cellList.size)

    var prev: Option[Pos] = Option.empty
    var emptyNb           = 0

    def writeEmptyNb() = if (emptyNb > 0) {
      res.append(s"$emptyNb")
      emptyNb = 0
    }

    board.variant.boardType.cellList.foreach(a => {
      if (prev.isDefined && prev.get.y != a.y) {
        writeEmptyNb()
        res.append("/")
      }

      board.getPiece(a) match {
        case None        => emptyNb += 1
        case Some(piece) =>
          writeEmptyNb()
          res.append(
            if (piece.player == P1) piece.forsyth.toString.toUpperCase()
            else piece.forsyth.toString.toLowerCase()
          )
      }

      prev = Option(a);
    })

    writeEmptyNb()

    res.toString
  }

  def boardAndPlayer(sit: Situation): String = boardAndPlayer(sit.board, sit.player)

  def boardAndPlayer(board: Board, nextPlayer: Player): String = s"${exportBoard(board)} ${nextPlayer.letter}"
}
