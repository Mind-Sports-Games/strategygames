package strategygames.abalone.format

import strategygames.abalone._
import strategygames.abalone.variant.{Abalone, GrandAbalone, Variant}
import strategygames.{Player, Score}

/** Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  */
object Forsyth {
  val initial = FEN("SS1ss/SSSsss/1SS1ss1/8/9/8/1ss1SS1/sssSSS/ss1SS 0 0 b 0 1") //TODO should NOT be here (I suppose)

  def <<@(variant: Variant, fen: FEN): Option[Situation] = {
    val pp = FEN.hasPrevPlayer(variant)
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
      case _ => if (allowNone) None else sys.error("Invalid player in fen")
    }

  def <<(fen: FEN): Option[Situation] = <<@(Variant.default, fen)

  case class SituationPlus(sit: Situation, fullTurnCount: Int) {
    def turnCount = sit.board.variant match {
      case Abalone => fullTurnCount * 2 - sit.player.fold(2, 1)
      case GrandAbalone => fullTurnCount * 2 - sit.player.fold(2, 1)//TODO Grand Abalone
    }

    // when we get a multiaction variant we should set this
    def plies = turnCount//TODO Grand Abalone
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
    val prevPlayer = if (FEN.hasPrevPlayer(game.situation.board.variant)) {
      game.situation.board.history.prevPlayer.fold(" 0")(p => p.fold(" b", " w"))
    } else ""
    val player = game.situation.player.fold('b', 'w')
    val halfMoves = game.situation.board.history.halfMoveClock
    val fullMoves = game.fullTurnCount
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