package strategygames.fairysf.format

import cats.implicits._

import strategygames.Color
import strategygames.fairysf._
import strategygames.fairysf.variant.{ Variant }

/** Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  *
  * Crazyhouse & Threecheck extensions:
  * https://github.com/ddugovic/Stockfish/wiki/FEN-extensions
  * http://scidb.sourceforge.net/help/en/FEN.html#ThreeCheck
  */
object Forsyth {

  //lishogi
  //val initial = FEN("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL b - 1")
  //pychess shogi
  val initial = FEN("lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL[-] w 0 1")

  //stub
  def <<@(variant: Variant, fen: FEN): Option[Situation] = {
    val apiPosition = Api.positionFromVariantNameAndFEN(variant.fairysfName.name, fen.value)
    Some(Situation(
      Board(
        pieces = apiPosition.pieceMap,
        history = History(),
        variant = variant,
        pocketData = apiPosition.pocketData,
        position = apiPosition.some
      ),
      fen.value.split(' ')(1) match {
        case "w" => White
        case "b" => Black
        case _ => sys.error("Invalid color in fen")
      }
    ))
  }

  def <<(fen: FEN): Option[Situation] = <<@(Variant.default, fen)

  case class SituationPlus(situation: Situation, fullMoveNumber: Int) {

    def turns = fullMoveNumber * 2 - situation.color.fold(2, 1)
  }

  def <<<@(variant: Variant, fen: FEN): Option[SituationPlus] =
    <<@(variant, fen) map { sit =>
      SituationPlus(
        //not doing half move clock history like we do in chess
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

  def boardAndColor(situation: Situation): String =
    boardAndColor(situation.board, situation.color)

  def boardAndColor(board: Board, turnColor: Color): String =
    s"${exportBoard(board)} ${turnColor.letter}"
}
