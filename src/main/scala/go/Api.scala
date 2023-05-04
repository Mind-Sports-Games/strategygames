package strategygames.go

import com.joansala.game.go.GoGame
import com.joansala.game.go.GoBoard

import cats.implicits._

import strategygames.Player
import strategygames.go.format.FEN
import strategygames.go.Pos
import strategygames.go.variant.Variant
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
    def goDiagram: String

    val fen: FEN
    val pieceMap: PieceMap

    val gameResult: GameResult
    val gameEnd: Boolean
    val gameOutcome: Int
    val legalMoves: Array[Int]
    val playerTurn: Int // 1 for South -1 for North
    def fenString: String
  }

  private class GoPosition(
      position: GoGame,
      ply: Int = 0,
      fromFen: Option[FEN] = None
  ) extends Position {
    // TODO: yes, this is an abuse of scala. We could get an
    //       exception here, but I'm not sure how to work around that
    //       at the moment
    val variant = Variant.byKey("go19x19") // todo change for size?

    def makeMovesWithPrevious(movesList: List[Int], previousMoves: List[String]): Position = {

      var pos =
        if (previousMoves.length == 0 && Api.initialFen.value != fen.value) positionFromFen(fen.value)
        else if (Api.initialFen.value != initialFen.value) positionFromFen(initialFen.value)
        else new GoPosition(new GoGame(), 0, fromFen)

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
      return new GoPosition(position, ply + movesList.length, fromFen)
    }

    // helper
    def toBoard: String = position.toBoard.toString

    def setBoard(goBoard: GoBoard): Unit = position.setBoard(goBoard)

    def goDiagram: String = position.toBoard.toDiagram

    private def playerPitsToFen(pits: Array[String]): String =
      pits
        .map { part =>
          part match {
            case "0" => "1" // empty pit
            case _   =>
              part.toIntOption match {
                case Some(x: Int) => s"${x}S"
                case _            => sys.error(s"expected integer in go string of pits: $pits")
              }
          }
        }
        .mkString(",") + ","

    private def joinEmptyPits(pits: String, replacePits: List[(String, String)]): String =
      replacePits.foldLeft(pits)((fen, repPair) => fen.replace(repPair._1, repPair._2))

    def fenString: String = {
      val splitDiagram   = goDiagram.split('-')
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
        playerIndex match {
          case "p1" => currentP1Score + goDiagram.split('-').take(6).map(_.toInt).sum
          case "p2" => currentP2Score + goDiagram.split('-').drop(6).take(6).map(_.toInt).sum
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
      FEN(fenString).goStoneArray.zipWithIndex
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

    val playerTurn: Int = position.turn()

    val initialFen: FEN = fromFen.fold(Api.initialFen)(f => f)

  }

  def position: Position =
    new GoPosition(new GoGame())

  def positionFromVariant(variant: Variant): Position =
    variant.key match {
      case "go9x9"   => new GoPosition(new GoGame())
      case "go13x13" => new GoPosition(new GoGame())
      case "go19x19" => new GoPosition(new GoGame())
      case _         => new GoPosition(new GoGame())
    }

  def positionFromFen(fenString: String): Position = {
    val game = new GoGame()
    val fen  = FEN(fenString)
    game.setBoard(goBoardFromFen(fenString))
    new GoPosition(game, fen.ply.getOrElse(0), Some(fen))
  }

  def positionFromVariantNameAndFEN(variantName: String, fenString: String): Position = {
    val game = new GoGame()
    val fen  = FEN(fenString)
    game.setBoard(goBoardFromFen(fenString))
    variantName.toLowerCase() match {
      case "go19x19" => new GoPosition(game, fen.ply.getOrElse(0), Some(fen))
      case _         => new GoPosition(new GoGame(), fen.ply.getOrElse(0), Some(fen))
    }
  }

  def goBoardFromFen(fenString: String): GoBoard = {
    // todo
    // val fen                    = FEN(fenString)
    // val posFromFen: Array[Int] = fen.goStoneArray :+ fen.player1Score :+ fen.player2Score
    // val turn: Int              = if (fen.player.map(_.p1).getOrElse(false)) 1 else -1
    new GoBoard()
  }

  def positionFromVariantAndMoves(variant: Variant, uciMoves: List[String]): Position =
    positionFromVariant(variant).makeMoves(uciMoves.map(m => uciToMove(m)))

  // TODO check 0 based coordinates
  def uciToMove(uciMove: String): Int = Pos.fromKey(uciMove takeRight 2).map(_.index).getOrElse(0)

  def moveToUci(move: Int): String = Pos(move).map(_.key).getOrElse("a1")

  val initialFen: FEN = variant.Go19x19.initialFen

  private val fenRegex                        = "([0-9]+[A-Z]?,?){1,19}(/([0-9]+[A-Z]?,?){1,19}){8,18} [0-9]+ [0-9]+ [N|S] [0-9]+"
  def validateFEN(fenString: String): Boolean =
    Try(goBoardFromFen(fenString)).isSuccess && fenString.matches(fenRegex)

  //  def positionFromMoves(variantName: String, fen: String, movesList: Option[List[String]] = None): Position =
  //    positionFromVariantNameAndFEN(variantName, fen)
  //      .makeMoves(convertUciMoves(movesList).getOrElse(List.empty))
  //

  def pieceMapFromFen(variantName: String, fenString: String): PieceMap = {
    positionFromVariantNameAndFEN(variantName, fenString).pieceMap
  }

}
