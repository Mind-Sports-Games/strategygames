package strategygames.fairysf.format

import cats.implicits._

import strategygames.Player
import strategygames.fairysf._
import strategygames.fairysf.variant.Variant

/** Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  */
object Forsyth {

  // lishogi
  // val initial = FEN("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
  // pychess shogi
  val initial = FEN("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL[] w - - 0 1")

  def <<@(variant: Variant, fen: FEN): Option[Situation] = {
    val apiPosition = Api.positionFromVariantNameAndFEN(variant.fairysfName.name, fen.value)
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
          case "w" => P1
          case "b" => P2
          case _   => sys.error("Invalid player in fen")
        }
      )
    )
  }

  def <<(fen: FEN): Option[Situation] = <<@(Variant.default, fen)

  case class SituationPlus(situation: Situation, fullTurnCount: Int) {

    def turnCount = fullTurnCount * 2 - situation.player.fold(2, 1)
    // TODO: Set this for Amazons. Needs to have the prev move encoded into the FEN
    // to calculate this. This work has been done here:
    // https://github.com/Mind-Sports-Games/strategygames/compare/master...392-support-amazons-in-analysis-page#diff-b0b2b8a0e93f9c1f675971f22eda79cc6cfedee6f2284b31a0fba54832966933R178
    // but hasn't been merged yet
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

  def >>(game: Game): FEN = exportBoardFen(game.situation.board)

  def exportBoard(board: Board): String = exportBoardFen(board).value

  def exportBoardFen(board: Board): FEN = board.variant.exportBoardFen(board)

  def boardAndPlayer(situation: Situation): String =
    boardAndPlayer(situation.board, situation.player)

  def boardAndPlayer(board: Board, turnPlayer: Player): String =
    s"${exportBoard(board)} ${turnPlayer.letter}"
}
