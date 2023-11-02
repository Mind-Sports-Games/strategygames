package strategygames.go.format

import cats.implicits._

import strategygames.Player
import strategygames.go._
import strategygames.go.variant.Variant

/** Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  */
object Forsyth {

  val initial = FEN(
    "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19[SSSSSSSSSSssssssssss] b - 0 75 0 0 75 1"
  )

  def <<@(variant: Variant, fen: FEN): Option[Situation] = {
    val apiPosition = Api.positionFromVariantNameAndFEN(variant.key, fen.value)
    Some(
      Situation(
        Board(
          pieces = apiPosition.pieceMap,
          history = History(),
          variant = variant,
          pocketData = apiPosition.pocketData,
          position = apiPosition.some
        ),
        fen.value.split(' ')(1) match {
          case "b" => P1
          case "w" => P2
          case _   => sys.error("Invalid player in fen")
        }
      )
    )
  }

  def <<(fen: FEN): Option[Situation] = <<@(fen.variant, fen)

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

  def <<<(fen: FEN): Option[SituationPlus] = <<<@(fen.variant, fen)

  def >>(situation: Situation): FEN = >>(SituationPlus(situation, 1))

  def >>(parsed: SituationPlus): FEN =
    parsed match {
      case SituationPlus(situation, _) =>
        >>(Game(situation, plies = parsed.plies, turnCount = parsed.turnCount))
    }

  def >>(game: Game): FEN = exportBoardFen(game.situation.board)

  // TODO Should this just be returning the board part of the fen? Check what Chess does
  def exportBoard(board: Board): String = exportBoardFen(board).value

  def exportBoardFen(board: Board): FEN =
    FEN(
      board.apiPosition.fen.value
        .split(" ")
        .updated(5, board.history.captures.p1)
        .updated(6, board.history.captures.p2)
        .mkString(" ")
    )

  def boardAndPlayer(situation: Situation): String =
    boardAndPlayer(situation.board, situation.player)

  // TODO review this, not sure this is correct as will return full fen appended with w/b
  def boardAndPlayer(board: Board, turnPlayer: Player): String =
    s"${exportBoard(board)} ${turnPlayer.letter}"
}
