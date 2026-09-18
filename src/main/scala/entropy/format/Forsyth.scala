package strategygames.entropy.format

import strategygames.{ Player, Pocket, Pockets, Score }
import strategygames.entropy._
import strategygames.entropy.variant.Variant

/** Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  */
object Forsyth {

  val initial = FEN("7/7/7/7/7/7/7[] w 0 0 1 1")

  def <<@(variant: Variant, fen: FEN): Option[Situation] =
    Some(
      Situation(
        Board(
          pieces = fen.pieces,
          history = History(
            score = Score(fen.player1Score, fen.player2Score),
            round = fen.round
          ),
          variant = variant,
          pocketData = Some(pocketDataOf(fen))
        ),
        fen.player.getOrElse(P1)
      )
    )

  private def pocketDataOf(fen: FEN): PocketData = {
    val held = fen.pocketRoles
    PocketData(
      Pockets(
        Pocket(held.collect { case (Player.P1, r) => strategygames.Role.EntropyRole(r) }),
        Pocket(held.collect { case (Player.P2, r) => strategygames.Role.EntropyRole(r) })
      )
    )
  }

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
    FEN(
      s"${boardPart(board)}${pocketPart(board)} ${player} ${board.history.score.fenStr} ${board.round} ${game.fullTurnCount}"
    )
  }

  def exportBoard(board: Board): String = s"${boardPart(board)}${pocketPart(board)}"

  def boardPart(board: Board): String = {
    val fen   = new scala.collection.mutable.StringBuilder(64)
    var empty = 0
    for (y <- Rank.allReversed) {
      empty = 0
      for (x <- File.all) {
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

  def pocketPart(board: Board): String =
    board.pocketData.fold("[]") { pd =>
      val held = Player.all.flatMap { p =>
        pd.pockets(p).roles.collect { case strategygames.Role.EntropyRole(r) =>
          Piece(p, r).forsyth
        }
      }
      s"[${held.mkString}]"
    }

  def boardAndPlayer(situation: Situation): String =
    boardAndPlayer(situation.board, situation.player)

  def boardAndPlayer(board: Board, turnPlayer: Player): String =
    s"${exportBoard(board)} ${turnPlayer.letter}"

}
