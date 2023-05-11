package strategygames.go

import com.joansala.game.go.GoGame
import com.joansala.game.go.GoBoard

import cats.implicits._

import strategygames.Player
import strategygames.go.format.FEN
import strategygames.go.Pos
import strategygames.go.variant.Variant
import scala.util.Try

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
    val playerTurn: Int // 1 for South (P1/black) -1 for North (P2/white)
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

    def fenString: String = {
      val splitDiagram = goDiagram.split(' ')
      val board        = splitDiagram.lift(0).getOrElse(" board fen error ")
      val turn         = splitDiagram.lift(1).getOrElse("w").toString()
      val ko           = splitDiagram.lift(2).getOrElse("-").toString()
      val p1Komi       = 0
      val p2Komi       = 6 // do we handle this here?
      val p1FinalScore = p1Komi
      val p2FinalScore = p2Komi
      val fullMoveStr  = (ply / 2 + 1).toString()
      return s"${board} ${turn} ${ko} ${p1FinalScore} ${p2FinalScore} ${fullMoveStr}"
    }

    def toPosition = position.toBoard().position()

    lazy val fen: FEN = FEN(fenString)

    private def convertPieceMapFromFen(fenString: String): PieceMap = {
      val boardWidth = variant.boardSize.width
      val boardFen   = fenString.split(' ').take(1).mkString("")
      var pieces     = Map.empty[Pos, Piece]
      boardFen
        .split('/')
        .zipWithIndex
        .map {
          case (row, rowIndex) => {
            var colIndex    = 0
            var isLastChar1 = false
            row.map(c => {
              c match {
                case 'X' =>
                  Pos((boardWidth - rowIndex - 1) * boardWidth + colIndex).map { pos =>
                    {
                      pieces += (pos -> Piece(P1, Stone))
                      colIndex += 1
                    }
                  }
                case 'O' =>
                  Pos((boardWidth - rowIndex - 1) * boardWidth + colIndex).map { pos =>
                    {
                      pieces += (pos -> Piece(P2, Stone))
                      colIndex += 1
                    }
                  }
                case n   => {
                  colIndex += n.asDigit
                  if (isLastChar1) colIndex += 9
                }
                case _   => sys.error(s"unrecognaised character in Go fen, ${c}")
              }
              isLastChar1 = c == '1'
            })
          }
        }

      pieces
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

  // todo handle constants in go engine for different size boards
  def positionFromVariant(variant: Variant): Position =
    variant.key match {
      case "go9x9"   => new GoPosition(new GoGame()) // todo setup 9x9
      case "go13x13" => new GoPosition(new GoGame()) // todo setup 13x13
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
    val fen = FEN(fenString)
    val b   = new GoBoard()
    val b2  = b.toBoard(fen.engineFen)
    b2
  }

  def positionFromVariantAndMoves(variant: Variant, uciMoves: List[String]): Position =
    positionFromVariant(variant).makeMoves(uciMoves.map(m => uciToMove(m)))

  // TODO check 0 based coordinates
  def uciToMove(uciMove: String): Int = Pos.fromKey(uciMove takeRight 2).map(_.index).getOrElse(0)

  def moveToUci(move: Int): String = Pos(move).map(_.key).getOrElse("a1")

  val initialFen: FEN = variant.Go19x19.initialFen

  val fenRegex                                = "([0-9XO]?){1,19}(/([0-9XO]?){1,19}){8,18} [w|b] - [0-9]+ [0-9]+ [0-9]+"
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
