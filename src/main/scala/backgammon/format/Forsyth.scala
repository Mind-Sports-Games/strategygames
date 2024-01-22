package strategygames.backgammon.format

import cats.implicits._

import strategygames.{ Player, Score }
import strategygames.backgammon._
import strategygames.backgammon.variant.Variant

/** Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  */
object Forsyth {

  val initial = FEN("5S,3,3s,1,5s,4,2S/5s,3,3S,1,5S,4,2s[] 0 0 w - - 1")

  def <<@(variant: Variant, fen: FEN): Option[Situation] = {
    Some(
      Situation(
        Board(
          pieces = fen.pieces,
          history = History(),
          variant = variant
        ),
        fen.value.split(' ')(3) match {
          case "w" => P1
          case "b" => P2
          case _   => sys.error("Invalid player in fen")
        }
      ).withHistory(
        History(
          score = Score(fen.player1Score, fen.player2Score),
          // seems like we might be using History.halfMoveClock incorrectly
          halfMoveClock = fen.fullMove.getOrElse(0)
        )
      )
    )
  }

  def <<(fen: FEN): Option[Situation] = <<@(Variant.default, fen)

  case class SituationPlus(situation: Situation, fullTurnCount: Int) {

    def turnCount = fullTurnCount * 2 - situation.player.fold(2, 1)
    // when we get a multiaction variant we should set this
    def plies     = turnCount

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

  def >>(situation: Situation): FEN = >>(SituationPlus(situation, 1))

  def >>(parsed: SituationPlus): FEN =
    parsed match {
      case SituationPlus(situation, _) =>
        >>(Game(situation, plies = parsed.plies, turnCount = parsed.turnCount))
    }

  def >>(game: Game): FEN = {
    val boardFen   = boardPart(game.situation.board)
    val pocketFen  = pocketPart(game.situation.board)
    val scoreStr   = game.situation.board.history.score.fenStr
    val player     = game.situation.player.fold('w', 'b')
    val unusedDice = game.situation.board.unusedDiceStr
    val usedDice   = game.situation.board.usedDiceStr
    val moves      = game.situation.board.history.halfMoveClock
    FEN(s"${boardFen}[${pocketFen}] ${scoreStr} ${player} ${unusedDice} ${usedDice} ${moves}")
  }

  def exportBoard(board: Board): String = {
    val boardFen  = boardPart(board)
    val pocketFen = pocketPart(board)
    val scoreStr  = board.history.score.fenStr
    s"${boardFen}[${pocketFen}] ${scoreStr}"
  }

  def boardPart(board: Board): String = {
    val fen   = new scala.collection.mutable.StringBuilder(70)
    var empty = 0
    for (y <- Rank.all) {
      empty = 0
      val files = if (y.index == 0) File.allReversed else File.all
      for (x <- files) {
        board(x, y) match {
          case None                 => empty = empty + 1
          case Some((piece, count)) =>
            if (empty > 0) {
              fen.append(s"${empty},")
              empty = 0
            }
            if (piece.role == Role.defaultRole)
              fen.append(s"${count}${piece.forsyth.toString},")
            else fen.append(s"${piece.forsyth},")
        }
      }
      if (empty > 0) fen.append(s"${empty},")
      fen.append('/')
    }
    fen.toString.replace(",/", "/").dropRight(1)
  }

  def pocketPart(board: Board) =
    board.pocketData match {
      case Some(PocketData(pockets)) =>
        pockets.p1.roles.map(_.forsyth).map(_.toUpper).mkString +
          pockets.p2.roles.map(_.forsyth).mkString
      case _                         => ""
    }

  def boardAndPlayer(situation: Situation): String =
    boardAndPlayer(situation.board, situation.player)

  def boardAndPlayer(board: Board, turnPlayer: Player): String =
    s"${exportBoard(board)} ${turnPlayer.letter}"

}
