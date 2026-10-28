package strategygames.fiveinarow.format

import strategygames.Player
import strategygames.fiveinarow._
import strategygames.fiveinarow.variant.Variant

object Forsyth {

  def initial(variant: Variant): FEN = {
    val size  = variant.boardSize
    val board = List.fill(size.height)(size.width.toString).mkString("/")
    val score = if (variant.hasGameScore) " 0 0" else ""
    FEN(s"$board b 1 ${variant.opening.initialStep.fen}$score 1")
  }

  val initial: FEN = initial(Variant.default)

  def <<@(variant: Variant, fen: FEN): Option[Situation] = {
    val board = fen.board(variant)
    if (!fen.boardSize.contains(variant.boardSize)) None
    else if (fen.nextColour.exists(_ != board.nextColour)) None
    else Some(Situation(board, board.playerToMove))
  }

  def <<(fen: FEN): Option[Situation] = Variant.forFen(fen).flatMap(<<@(_, fen))

  case class SituationPlus(situation: Situation, fullTurnCount: Int) {

    def turnCount = fullTurnCount * 2 - situation.player.fold(2, 1)
    def plies     = turnCount

  }

  def <<<@(variant: Variant, fen: FEN): Option[SituationPlus] =
    <<@(variant, fen) map { sit =>
      SituationPlus(sit, fen.fullMove.map(_ max 1) getOrElse 1)
    }

  def <<<(fen: FEN): Option[SituationPlus] =
    Variant.forFen(fen).flatMap(<<<@(_, fen))

  def >>(situation: Situation): FEN = >>(SituationPlus(situation, 1))

  def >>(parsed: SituationPlus): FEN =
    >>(Game(parsed.situation, plies = parsed.plies, turnCount = parsed.turnCount))

  def >>(game: Game): FEN = {
    val board = game.situation.board
    FEN(s"${boardPart(board)} ${stateFields(board)} ${game.fullTurnCount}")
  }

  private def stateFields(board: Board): String = {
    val score = if (board.variant.hasGameScore) s" ${board.score.fenStr}" else ""
    s"${board.nextColour.forsyth.toLower} ${board.blackSeat.fold(1, 2)} ${board.openingStep.fen}$score"
  }

  def exportBoard(board: Board): String = boardPart(board)

  def boardPart(board: Board): String =
    board.boardSize.ranks.reverse
      .map { y =>
        val (row, empty) = board.boardSize.files.foldLeft(("", 0)) { case ((row, empty), x) =>
          val point = Pos(x, y)
          board(point)
            .map(_.forsyth)
            .orElse(Option.when(board.offered.contains(point))(FEN.offeredChar)) match {
            case None    => (row, empty + 1)
            case Some(c) => (s"$row${if (empty > 0) empty else ""}$c", 0)
          }
        }
        if (empty > 0) s"$row$empty" else row
      }
      .mkString("/")

  def boardAndPlayer(situation: Situation): String =
    boardAndPlayer(situation.board, situation.player)

  def boardAndPlayer(board: Board, turnPlayer: Player): String =
    s"${exportBoard(board)} ${turnPlayer.letter}"

}
