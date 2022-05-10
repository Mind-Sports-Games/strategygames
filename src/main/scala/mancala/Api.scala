package strategygames.mancala

import com.joansala.game.oware.OwareGame
import com.joansala.game.oware.OwareBoard

import cats.implicits._

import strategygames.Player
import strategygames.mancala.format.FEN
import strategygames.mancala.variant.Variant
import scala.util.{ Failure, Success, Try }

sealed abstract class GameResult extends Product with Serializable

object GameResult {
  final case class VariantEnd() extends GameResult
  final case class Draw()       extends GameResult
  final case class Ongoing()    extends GameResult

  def resultFromInt(value: Int, ended: Boolean): GameResult =
    if (value.abs == 1000) GameResult.VariantEnd()
    else if (value == 0 && ended) GameResult.Draw()
    else if (value == 0 && !ended) GameResult.Ongoing()
    else sys.error(s"Unknown game result: ${value}")

}

object Api {

  abstract class Position {
    val variant: Variant

    def makeMoves(movesList: List[Int]): Position
    def makeMovesWithPrevious(movesList: List[Int], previousMoves: List[String]): Position

    def toBoard: String
    def owareDiagram: String

    val fen: FEN
    val pieceMap: PieceMap

    val gameResult: GameResult
    val gameEnd: Boolean
    val gameOutcome: Int
    val legalMoves: Array[Int]
    val playerTurn: Int // 1 for South -1 for North
    def fenString: String
  }

  private class OwarePosition(position: OwareGame) extends Position {
    // TODO: yes, this is an abuse of scala. We could get an
    //       exception here, but I'm not sure how to work around that
    //       at the moment
    val variant = Variant.byKey("oware")

    def makeMovesWithPrevious(movesList: List[Int], previousMoves: List[String]): Position = {
      val game = new OwareGame()
      previousMoves.foreach(uci => game.makeMove(uciToMove(uci)))

      movesList.map { move =>
        if (game.legalMoves.contains(move)) game.makeMove(move)
        else sys.error(s"Illegal move: ${move} from list: ${movesList}")
      }
      return new OwarePosition(game)
    }

    def makeMoves(movesList: List[Int]): Position = {
      movesList.map { move =>
        if (position.legalMoves.contains(move)) position.makeMove(move)
        else sys.error(s"Illegal move: ${move} from list: ${movesList}")
      }
      return new OwarePosition(position)
    }

    // helper
    def toBoard: String = position.toBoard.toString

    def setBoard(owareBoard: OwareBoard): Unit = position.setBoard(owareBoard)

    def owareDiagram: String = position.toBoard.toDiagram

    private val numHouses = variant.boardSize.width * variant.boardSize.height

    def seedCountToLetter(num: Int): Char =
      num match {
        case x if x < 27  => (x + 64).toChar
        case x if x >= 27 => (x + 70).toChar
        case x if x > 52  => sys.error("expected number of stones less than 53, got " + x.toString())
      }

    def scoreNumberToLetter(num: Int): String =
      num match {
        case 0 => "0"
        case x => seedCountToLetter(x).toString()
      }

    def fenString: String = {
      val board        = (
        owareDiagram.split('-').take(variant.boardSize.width * 2).drop(variant.boardSize.width).reverse
          ++
            owareDiagram.split('-').take(variant.boardSize.width)
      )
        .map { part =>
          part match {
            case "0" => "1" // empty house
            case _   =>
              part.toIntOption match {
                case Some(x: Int) => seedCountToLetter(x)
                case _            => sys.error("expected integer number of string in owareDiagram: " + owareDiagram)
              }
          }
        }
        .mkString("")
        .patch(variant.boardSize.width, "/", 0)
      val updatedBoard =
        "^(1+)$".r.replaceAllIn(board, "$1".length.toString()) // combine 1's into size of group
      val p1CurrentScore = owareDiagram.split('-')(numHouses).toInt
      val p2CurrentScore = owareDiagram.split('-')(numHouses + 1).toInt
      val p1FinalScore   = finalStoneScore(p1CurrentScore, p2CurrentScore, "p1")
      val p2FinalScore   = finalStoneScore(p1CurrentScore, p2CurrentScore, "p2")
      return s"${updatedBoard} ${scoreNumberToLetter(p1FinalScore)} ${scoreNumberToLetter(p2FinalScore)} ${owareDiagram
          .split('-')(numHouses + 2)}"
    }

    def finalStoneScore(currentP1Score: Int, currentP2Score: Int, playerIndex: String): Int = {
      if (position.hasEnded() && currentP1Score < 25 && currentP2Score < 25) {
        playerIndex match {
          case "p1" => currentP1Score + owareDiagram.split('-').take(6).map(_.toInt).sum
          case "p2" => currentP2Score + owareDiagram.split('-').drop(6).take(6).map(_.toInt).sum
        }
      } else {
        playerIndex match {
          case "p1" => currentP1Score
          case "p2" => currentP2Score
        }
      }
    }

    def toPosition = position.toBoard().position()

    lazy val fen: FEN = FEN(fenString)

    private def convertPieceMapFromFen(fenString: String): PieceMap = {
      FEN(fenString).owareStoneArray.zipWithIndex
        .map { case (seeds, index) =>
          seeds match {
            case 0 => (None, None)
            case n =>
              (
                Pos(index),
                Piece.fromStoneNumber(if (index < 6) Player.fromP1(true) else Player.fromP1(false), n)
              )
          }
        }
        .filter(x => x != (None, None))
        .map { case (Some(pos), Some(piece)) => pos -> piece }
        .toMap
    }

    lazy val pieceMap: PieceMap = convertPieceMapFromFen(fenString)

    lazy val gameResult: GameResult =
      GameResult.resultFromInt(position.outcome(), position.hasEnded())

    lazy val gameEnd: Boolean = position.hasEnded()

    lazy val gameOutcome: Int = position.outcome()

    val legalMoves: Array[Int] = {
      position.resetCursor()
      var moves: List[Int] = List()
      var nextMove         = position.nextMove()
      while (nextMove != -1) {
        moves = moves ::: List(nextMove)
        nextMove = position.nextMove()
      }
      moves.toArray
    }
    val playerTurn: Int        = position.turn()
  }

  def position: Position =
    new OwarePosition(new OwareGame())

  def positionFromVariant(variant: Variant): Position =
    variant.key match {
      case "oware" => new OwarePosition(new OwareGame())
      case _       => new OwarePosition(new OwareGame())
    }

  def positionFromFen(fenString: String): Position = {
    val game = new OwareGame()
    game.setBoard(owareBoardFromFen(fenString))
    new OwarePosition(game)
  }

  def positionFromVariantNameAndFEN(variantName: String, fenString: String): Position = {
    val game = new OwareGame()
    game.setBoard(owareBoardFromFen(fenString))
    variantName match {
      case "oware" => new OwarePosition(game)
      case _       => new OwarePosition(new OwareGame())
    }
  }

  def owareBoardFromFen(fenString: String): OwareBoard = {
    val myFen                  = FEN(fenString)
    val posFromFen: Array[Int] = myFen.owareStoneArray :+ myFen.player1Score :+ myFen.player2Score
    val turn: Int              = if (fenString.split(" ").last == "S") 1 else -1
    new OwareBoard(posFromFen, turn)
  }

  def positionFromVariantAndMoves(variant: Variant, uciMoves: List[String]): Position =
    positionFromVariant(variant).makeMoves(uciMoves.map(m => uciToMove(m)))

  // assumption for uci that 'a1' is bottom left for South player.
  def uciToMove(uciMove: String): Int = {
    uciMove(1).toString() match {
      case "1" => uciMove(0).toInt - 97
      case _   => 11 - (uciMove(0).toInt - 97)
    }
  }

  def moveToUci(move: Int): String = {
    move match {
      case x if x < 6 => s"${(x + 97).toChar}1"
      case _          => s"${((11 - move) + 97).toChar}2"
    }
  }

  val initialFen: FEN = variant.Oware.initialFen

  private val fenRegex                        = "[A-Za-z0-6]{1,6}/[A-Za-z0-6]{1,6} [A-Za-z0]+ [A-Za-z0]+ [N|S]"
  def validateFEN(fenString: String): Boolean =
    Try(owareBoardFromFen(fenString)).isSuccess && fenString.matches(fenRegex)

  //  def positionFromMoves(variantName: String, fen: String, movesList: Option[List[String]] = None): Position =
  //    positionFromVariantNameAndFEN(variantName, fen)
  //      .makeMoves(convertUciMoves(movesList).getOrElse(List.empty))
  //

  def pieceMapFromFen(variantName: String, fenString: String): PieceMap = {
    positionFromVariantNameAndFEN(variantName, fenString).pieceMap
  }

}
