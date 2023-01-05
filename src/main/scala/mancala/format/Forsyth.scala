package strategygames.mancala.format

import cats.implicits._

import strategygames.Player
import strategygames.mancala._
import strategygames.mancala.variant.Variant

/** Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  */
object Forsyth {

  val initial = FEN("4S,4S,4S,4S,4S,4S/4S,4S,4S,4S,4S,4S 0 0 S 0")

  def <<@(variant: Variant, fen: FEN): Option[Situation] = {
    val apiPosition = Api.positionFromVariantNameAndFEN(variant.name, fen.value)
    Some(
      Situation(
        Board(
          pieces = apiPosition.pieceMap,
          history = History(),
          variant = variant,
          position = apiPosition.some
        ),
        fen.value.split(' ')(3) match {
          case "S" => P1
          case "N" => P2
          case _   => sys.error("Invalid player in fen")
        }
      )
    )
  }

  def <<(fen: FEN): Option[Situation] = <<@(Variant.default, fen)

  case class SituationPlus(situation: Situation, fullMoveNumber: Int) {

    def turns = fullMoveNumber * 2 - situation.player.fold(2, 1)
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
      case SituationPlus(situation, _) => >>(Game(situation, turns = parsed.turns))
    }

  def >>(game: Game): FEN = exportBoardFen(game.situation.board)

  def exportBoard(board: Board): String = exportBoardFen(board).value

  def exportBoardFen(board: Board): FEN = board.apiPosition.fen

  def boardAndPlayer(situation: Situation): String =
    boardAndPlayer(situation.board, situation.player)

  def boardAndPlayer(board: Board, turnPlayer: Player): String =
    s"${exportBoard(board)} ${turnPlayer.letter}"
}
