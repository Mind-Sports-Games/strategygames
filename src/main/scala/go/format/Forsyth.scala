package strategygames.go.format

import cats.implicits._

import strategygames.Score
import strategygames.Player
import strategygames.go._
import strategygames.go.variant.Variant

/** Transform a game to standard Forsyth Edwards Notation
  * http://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  */
object Forsyth {

  val initial = FEN(
    "19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19/19[SSSSSSSSSSssssssssss] b - 0 75 0 0 75 0 1"
  )

  def <<@(variant: Variant, fen: FEN): Option[Situation] = {
    val apiPosition = Api.positionFromVariantNameAndFEN(variant.key, fen.value)
    Some(
      Situation(
        Board(
          pieces = apiPosition.pieceMap,
          history = History(captures = Score(fen.player1Captures, fen.player2Captures)),
          variant = variant,
          pocketData = apiPosition.pocketData,
          uciMoves = fen.fenPassCount match {
            case 0 => List()
            case 1 => List("pass")
            case 2 => List("pass", "pass")
            case 3 => List("ss:")
          },
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
        // Update player if we have a last action of select squares that the API doesnt know about
        .updated(
          1,
          board.history.lastTurn.headOption match {
            case Some(_: Uci.SelectSquares) if board.apiPosition.turn == "w" => "b"
            case Some(_: Uci.SelectSquares) if board.apiPosition.turn == "b" => "w"
            case _                                                           => board.apiPosition.turn
          }
        )
        // Update captures
        .updated(5, board.history.captures.p1.toString)
        .updated(6, board.history.captures.p2.toString)
        // Update current consecutive Pass count. Use 3 to represent end of game
        .updated(
          8,
          (board.uciMoves.reverse.headOption match {
            case Some(uci) if uci.startsWith("ss:") => 3
            case Some(uci) if uci == "pass"         => {
              board.uciMoves.reverse.drop(1).headOption match {
                case Some(uci) if uci == "pass" => 2
                case _                          => 1
              }
            }
            case _                                  => 0
          }).toString
        )
        // Update fullTurnCount if we have a last action of select squares that the API doesnt know about
        .updated(
          9,
          board.history.lastTurn.headOption match {
            case Some(_: Uci.SelectSquares) if board.apiPosition.turn == "w" =>
              board.apiPosition.fen.value.split(" ")(9) + 1
            case _                                                           => board.apiPosition.fen.value.split(" ")(9)
          }
        )
        .mkString(" ")
    )

  def boardAndPlayer(situation: Situation): String =
    boardAndPlayer(situation.board, situation.player)

  // TODO review this, not sure this is correct as will return full fen appended with w/b
  def boardAndPlayer(board: Board, turnPlayer: Player): String =
    s"${exportBoard(board)} ${turnPlayer.letter}"
}
