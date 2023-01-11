package strategygames.samurai

import com.joansala.game.oware.OwareGame
import com.joansala.game.oware.OwareBoard

import cats.implicits._

import strategygames.Player
import strategygames.samurai.format.FEN
import strategygames.samurai.variant.Variant
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
    val isRepetition: Boolean
    val legalMoves: Array[Int]
    val playerTurn: Int // 1 for South -1 for North
    def fenString: String
  }

  private class OwarePosition(
      position: OwareGame,
      ply: Int = 0,
      fromFen: Option[FEN] = None
  ) extends Position {
    // TODO: yes, this is an abuse of scala. We could get an
    //       exception here, but I'm not sure how to work around that
    //       at the moment
    val variant = Variant.byKey("oware")

    def makeMovesWithPrevious(movesList: List[Int], previousMoves: List[String]): Position = {

      var pos =
        if (previousMoves.length == 0 && Api.initialFen.value != fen.value) positionFromFen(fen.value)
        else if (Api.initialFen.value != initialFen.value) positionFromFen(initialFen.value)
        else new OwarePosition(new OwareGame(), 0, fromFen)

      pos = pos.makeMoves(previousMoves.map(uciToMove))

      movesList.map { move =>
        if (pos.legalMoves.contains(move)) pos = pos.makeMoves(List(move))
        else
          sys.error(
            s"Illegal move1: ${move} from list: ${movesList} legalMoves: ${pos.legalMoves.map(_.toString()).mkString(", ")}"
          )
      }
      return pos
    }

    def makeMoves(movesList: List[Int]): Position = {
      movesList.map { move =>
        if (position.legalMoves.contains(move)) position.makeMove(move)
        else
          sys.error(
            s"Illegal move2: ${move} from list: ${movesList} legalMoves: ${position.legalMoves.map(_.toString()).mkString(", ")}"
          )
      }
      return new OwarePosition(position, ply + movesList.length, fromFen)
    }

    // helper
    def toBoard: String = position.toBoard.toString

    def setBoard(owareBoard: OwareBoard): Unit = position.setBoard(owareBoard)

    def owareDiagram: String = position.toBoard.toDiagram

    private def playerPitsToFen(pits: Array[String]): String =
      pits
        .map { part =>
          part match {
            case "0" => "1" // empty pit
            case _   =>
              part.toIntOption match {
                case Some(x: Int) => s"${x}S"
                case _            => sys.error(s"expected integer in oware string of pits: $pits")
              }
          }
        }
        .mkString(",") + ","

    private def joinEmptyPits(pits: String, replacePits: List[(String, String)]): String =
      replacePits.foldLeft(pits)((fen, repPair) => fen.replace(repPair._1, repPair._2))

    def fenString: String = {
      val splitDiagram   = owareDiagram.split('-')
      val width          = variant.boardSize.width
      val numPits        = variant.boardSize.width * variant.boardSize.height
      val replacePits    = (2 to width).toList.reverse.map(i => ("1," * i, s"$i,"))
      val board          = List(
        splitDiagram.take(width * 2).drop(width).reverse,
        splitDiagram.take(width)
      ).map(playerPitsToFen)
        .map(pits => joinEmptyPits(pits, replacePits))
        .map(_.dropRight(1))
        .mkString("/")
      val p1CurrentScore = splitDiagram(numPits).toInt
      val p2CurrentScore = splitDiagram(numPits + 1).toInt
      val p1FinalScore   = finalStoneScore(p1CurrentScore, p2CurrentScore, "p1")
      val p2FinalScore   = finalStoneScore(p1CurrentScore, p2CurrentScore, "p2")
      val fullMoveStr    = (ply / 2 + 1).toString()
      return s"${board} ${p1FinalScore} ${p2FinalScore} ${splitDiagram(numPits + 2)} ${fullMoveStr}"
    }

    private def finalStoneScore(currentP1Score: Int, currentP2Score: Int, playerIndex: String): Int = {
      if (position.hasEnded() && currentP1Score < 25 && currentP2Score < 25) {
        if (position.isRepetition()) {
          val p1SideStonesLeft = owareDiagram.split('-').take(6).map(_.toInt).sum
          val p2SideStonesLeft = owareDiagram.split('-').drop(6).take(6).map(_.toInt).sum
          val total            = p1SideStonesLeft + p2SideStonesLeft
          total match {
            case _ if total % 2 == 0 =>
              playerIndex match {
                case "p1" => currentP1Score + total / 2
                case "p2" => currentP2Score + total / 2
              }
            case _ =>
              if (p1SideStonesLeft > p2SideStonesLeft) {
                playerIndex match {
                  case "p1" => currentP1Score + (total + 1) / 2
                  case "p2" => currentP2Score + (total - 1) / 2
                }
              } else {
                playerIndex match {
                  case "p1" => currentP1Score + (total - 1) / 2
                  case "p2" => currentP2Score + (total + 1) / 2
                }
              }
          }
        } else {
          playerIndex match {
            case "p1" => currentP1Score + owareDiagram.split('-').take(6).map(_.toInt).sum
            case "p2" => currentP2Score + owareDiagram.split('-').drop(6).take(6).map(_.toInt).sum
          }
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
        .filterNot { case (s, _) => s == 0 }
        .map { case (seeds, index) =>
          (Pos(index), (Piece(Player.fromP1(index < 6), variant.defaultRole), seeds))
        }
        .map { case (Some(pos), pieceCount) => pos -> pieceCount }
        .toMap
    }

    lazy val pieceMap: PieceMap = convertPieceMapFromFen(fenString)

    lazy val gameResult: GameResult =
      GameResult.resultFromInt(position.outcome(), position.hasEnded())

    lazy val gameEnd: Boolean = position.hasEnded()

    lazy val gameOutcome: Int      = position.outcome()
    lazy val isRepetition: Boolean = position.isRepetition()

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

    val playerTurn: Int = position.turn()

    val initialFen: FEN = fromFen.fold(Api.initialFen)(f => f)

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
    val fen  = FEN(fenString)
    game.setBoard(owareBoardFromFen(fenString))
    new OwarePosition(game, fen.ply.getOrElse(0), Some(fen))
  }

  def positionFromVariantNameAndFEN(variantName: String, fenString: String): Position = {
    val game = new OwareGame()
    val fen  = FEN(fenString)
    game.setBoard(owareBoardFromFen(fenString))
    variantName.toLowerCase() match {
      case "oware" => new OwarePosition(game, fen.ply.getOrElse(0), Some(fen))
      case _       => new OwarePosition(new OwareGame(), fen.ply.getOrElse(0), Some(fen))
    }
  }

  def owareBoardFromFen(fenString: String): OwareBoard = {
    val fen                    = FEN(fenString)
    val posFromFen: Array[Int] = fen.owareStoneArray :+ fen.player1Score :+ fen.player2Score
    val turn: Int              = if (fen.player.map(_.p1).getOrElse(false)) 1 else -1
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

  private val fenRegex                        = "([0-9]+[A-Z]?,?){1,6}/([0-9]+[A-Z]?,?){1,6} [0-9]+ [0-9]+ [N|S] [0-9]+"
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
