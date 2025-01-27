package strategygames.backgammon.format

import cats.implicits._
import scala.math.log

import strategygames.{ Player, Score }
import strategygames.backgammon._
import strategygames.backgammon.variant.Variant

/** Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  */
object Forsyth {

  val initial = FEN("5S,3,3s,1,5s,4,2S/5s,3,3S,1,5S,4,2s[] - - w 0 0 - 1")

  def <<@(variant: Variant, fen: FEN): Option[Situation] = {
    Some(
      Situation(
        Board(
          pieces = fen.pieces,
          pocketData = fen.pocketData,
          history = History(),
          variant = variant,
          unusedDice = fen.unusedDice,
          cubeData = fen.cubeData
        ),
        fen.value.split(' ')(3) match {
          case "w" => P1
          case "b" => P2
          case _   => sys.error("Invalid player in fen")
        }
      ).withHistory(
        History(
          currentTurn = List((fen.unusedDice ++ fen.usedDice).take(2))
            .filter(_.nonEmpty)
            .map(Uci.DiceRoll(_)),
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
    // This is incorrect, but does it matter? We haven't set this for Monster Chess
    // Think this is used when created fromPosition and we wouldn't necessarily need
    // to know the number of plies that have happened from before we start
    // See also fairysf equivalent
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
    val boardFen = exportBoard(game.situation.board)
    val player   = game.situation.player.fold('w', 'b')
    val scoreStr = game.situation.board.history.score.fenStr
    val cube     = cubePart(game.situation.board.cubeData)
    val moves    = game.fullTurnCount
    FEN(s"${boardFen} ${player} ${scoreStr} ${cube} ${moves}")
  }

  def exportBoard(board: Board): String =
    s"${boardPart(board)}[${pocketPart(board.pocketData)}] ${board.unusedDiceStr} ${board.usedDiceStr}"

  def boardPart(board: Board): String = {
    val fen   = new scala.collection.mutable.StringBuilder(70)
    var empty = 0
    for (y <- Rank.allReversed) {
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

  def pocketPart(pocketData: Option[PocketData]) =
    pocketData match {
      case Some(PocketData(pockets)) =>
        List(
          (if (pockets.p1.roles.isEmpty) None
           else Some(s"${pockets.p1.roles.size}${Role.defaultRole.forsythUpper}")),
          (if (pockets.p2.roles.isEmpty) None
           else Some(s"${pockets.p2.roles.size}${Role.defaultRole.forsyth}"))
        ).flatten.mkString(",")
      case _                         => ""
    }

  def cubePart(cubeData: Option[CubeData]) =
    cubeData match {
      case Some(cubeData) =>
        s"${(log(cubeData.value) / log(2)).toInt}${cubeData.owner
            .map(p => if (cubeData.underOffer) p.letter else p.letter.toUpper)
            .getOrElse("")}${if (cubeData.rejected) "x" else ""}"
      case _              => "-"
    }

  def boardAndPlayer(situation: Situation): String =
    boardAndPlayer(situation.board, situation.player)

  def boardAndPlayer(board: Board, turnPlayer: Player): String =
    s"${exportBoard(board)} ${turnPlayer.letter}"

}
