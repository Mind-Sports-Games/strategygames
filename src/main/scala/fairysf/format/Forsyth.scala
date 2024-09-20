package strategygames.fairysf.format

import cats.implicits._

import strategygames.{ GameFamily, Player }
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
    val (fsfFen, pieceMap, history) =
      // only used by Amazons (game with a ½ in the fen)
      if (fen.value.contains("½")) {
        val fenPieceMap      = makePieceMapFromFen(variant, fen)
        val uciMove          = Uci.Move.apply(variant.gameFamily, fen.value.dropWhile('½' !=).drop(1))
        val previousBoardFen = exportBoardFen(
          Board(
            // unapply ½ move
            pieces = uciMove match {
              case Some(uciMove) =>
                fenPieceMap.get(uciMove.dest) match {
                  case Some(piece) => fenPieceMap + ((uciMove.orig, piece)) - uciMove.dest
                  case None        => fenPieceMap
                }
              case None          => sys.error("Invalid ½ move")
            },
            variant = variant
          )
        )
        (
          FEN(
            (previousBoardFen.value.split(' ').headOption.toList ++ fen.value.split(' ').drop(1))
              .mkString(" ")
          ),
          Some(fenPieceMap),
          Some(History(currentTurn = uciMove.toList))
        )
      } else (fen, None, None)
    val apiPosition = Api.positionFromVariantNameAndFEN(variant.fairysfName.name, fsfFen.value)
    Some(
      Situation(
        Board(
          pieces = pieceMap.getOrElse(apiPosition.pieceMap),
          history = history.getOrElse(History()),
          variant = variant,
          // this is safe because no ½ move games require pocket piece updates
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

  // only cares about pieces positions on the board (first part of FEN string)
  // only used by Amazons (game with a ½ in the fen)
  private def makePieceMapFromFen(variant: Variant, fen: FEN): PieceMap = {

    def makePieceMap(
        chars: List[Char],
        gf: GameFamily,
        x: Int,
        y: Int
    ): Option[List[(Pos, Piece)]] =
      chars match {
        case Nil                               => Option(Nil)
        case '/' :: rest                       => makePieceMap(rest, gf, 0, y - 1)
        case '1' :: '0' :: rest                => makePieceMap(rest, gf, x + 10, y)
        case c :: rest if '1' <= c && c <= '9' => makePieceMap(rest, gf, x + (c - '0').toInt, y)
        case c :: rest                         =>
          for {
            pos        <- Pos.at(x, y)
            piece      <- Piece.fromChar(gf, c)
            nextPieces <- makePieceMap(rest, gf, x + 1, y)
          } yield (pos -> piece :: nextPieces)
      }

    makePieceMap(
      fen.value.takeWhile(' ' !=).takeWhile('[' !=).toList,
      variant.gameFamily,
      0,
      variant.boardSize.height - 1
    ).toList.flatten.toMap
  }

  case class SituationPlus(situation: Situation, fullTurnCount: Int) {

    def turnCount = fullTurnCount * 2 - situation.player.fold(2, 1)
    def plies     = situation.board.variant.pliesFromFen(
      fullTurnCount,
      situation.player,
      situation.board.history.currentTurn.size
    )
  }

  def <<<@(variant: Variant, fen: FEN): Option[SituationPlus] = {
    <<@(variant, fen) map { sit =>
      SituationPlus(
        // not doing half move clock history like we do in chess
        sit,
        fen.value.split(' ').lift(FEN.fullMoveIndex).flatMap(_.toIntOption).map(_ max 1 min 500) | 1
      )
    }
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
