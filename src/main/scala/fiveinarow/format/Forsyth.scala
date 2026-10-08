package strategygames.fiveinarow.format

import strategygames.Player
import strategygames.fiveinarow._
import strategygames.fiveinarow.variant.Variant

object Forsyth {

  val initial = FEN(s"${List.fill(Rank.allSize)(File.allSize.toString).mkString("/")} b 1 o 1")

  def <<@(variant: Variant, fen: FEN): Option[Situation] = {
    val board = fen.board(variant)
    if (fen.nextColour.exists(_ != board.nextColour)) None
    else Some(Situation(board, board.playerToMove))
  }

  def <<(fen: FEN): Option[Situation] = <<@(Variant.default, fen)

  case class SituationPlus(situation: Situation, fullTurnCount: Int) {

    def turnCount = fullTurnCount * 2 - situation.player.fold(2, 1)
    def plies     = turnCount

  }

  def <<<@(variant: Variant, fen: FEN): Option[SituationPlus] =
    <<@(variant, fen) map { sit =>
      SituationPlus(sit, fen.fullMove.map(_ max 1) getOrElse 1)
    }

  def <<<(fen: FEN): Option[SituationPlus] = <<<@(Variant.default, fen)

  def >>(situation: Situation): FEN = >>(SituationPlus(situation, 1))

  def >>(parsed: SituationPlus): FEN =
    >>(Game(parsed.situation, plies = parsed.plies, turnCount = parsed.turnCount))

  def >>(game: Game): FEN = {
    val board = game.situation.board
    FEN(s"${boardPart(board)} ${stateFields(board)} ${game.fullTurnCount}")
  }

  private def stateFields(board: Board): String =
    s"${board.nextColour.forsyth.toLower} ${board.blackSeat.fold(1, 2)} ${board.openingStep.fen}"

  def exportBoard(board: Board): String = boardPart(board)

  def boardPart(board: Board): String =
    Rank.allReversed
      .map { y =>
        val (row, empty) = File.all.foldLeft(("", 0)) { case ((row, empty), x) =>
          board(x, y) match {
            case None        => (row, empty + 1)
            case Some(piece) => (s"$row${if (empty > 0) empty else ""}${piece.forsyth}", 0)
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
