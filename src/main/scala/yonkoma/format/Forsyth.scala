package strategygames.yonkoma.format

import strategygames.Player
import strategygames.yonkoma._
import strategygames.yonkoma.variant.Variant

/** Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  */
object Forsyth {

  val initial = FEN("c8C/10/10/10/10/10/10/10/10/C8c w 1")

  def <<@(variant: Variant, fen: FEN): Option[Situation] =
    Some(
      Situation(
        Board(
          pieces = fen.pieces,
          history = History(),
          variant = variant
        ),
        fen.player.getOrElse(P1)
      )
    )

  def <<(fen: FEN): Option[Situation] = <<@(Variant.default, fen)

  case class SituationPlus(situation: Situation, fullTurnCount: Int) {

    def turnCount = fullTurnCount * 2 - situation.player.fold(2, 1)
    def plies     = turnCount

  }

  def <<<@(variant: Variant, fen: FEN): Option[SituationPlus] =
    <<@(variant, fen) map { sit =>
      SituationPlus(
        sit,
        fen.fullMove.map(_ max 1) getOrElse 1
      )
    }

  def <<<(fen: FEN): Option[SituationPlus] = <<<@(Variant.default, fen)

  def >>(situation: Situation): FEN = >>(SituationPlus(situation, 1))

  def >>(parsed: SituationPlus): FEN =
    parsed match {
      case SituationPlus(situation, _) =>
        >>(Game(situation, plies = parsed.plies, turnCount = parsed.turnCount))
    }

  def >>(game: Game): FEN = {
    val board  = game.situation.board
    val player = game.situation.player.fold('w', 'b')
    FEN(s"${boardPart(board)} ${player} ${game.fullTurnCount}")
  }

  def exportBoard(board: Board): String = boardPart(board)

  def boardPart(board: Board): String = {
    val fen   = new scala.collection.mutable.StringBuilder(64)
    var empty = 0
    val ranks = board.boardSize.ranks
    val files = board.boardSize.files
    for (y <- ranks.reverse) {
      empty = 0
      for (x <- files) {
        board(x, y) match {
          case None        => empty = empty + 1
          case Some(piece) =>
            if (empty > 0) {
              fen.append(empty.toString)
              empty = 0
            }
            fen.append(piece.forsyth.toString)
        }
      }
      if (empty > 0) fen.append(empty.toString)
      if (y != Rank.First) fen.append('/')
    }
    fen.toString
  }

  def boardAndPlayer(situation: Situation): String =
    boardAndPlayer(situation.board, situation.player)

  def boardAndPlayer(board: Board, turnPlayer: Player): String =
    s"${exportBoard(board)} ${turnPlayer.letter}"

}
