package strategygames.go

import com.joansala.game.go.GoGame
import com.joansala.game.go.GoBoard

import cats.implicits._

import strategygames.{ Player, Pocket, Pockets }
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
    if (value.abs == 1000 && ended) GameResult.VariantEnd()
    else if (value == 0 && ended) GameResult.Draw()
    else if (!ended) GameResult.Ongoing()
    else sys.error(s"Unknown game result: ${value}")

}

object Api {

  abstract class Position {
    val variant: Variant

    def makeMoves(movesList: List[Int]): Position
    def makeMovesWithPrevious(movesList: List[Int], previousMoves: List[String]): Position

    def toBoard: String
    def goDiagram: String
    def setKomi(komi: Int): Unit

    val fen: FEN
    val pieceMap: PieceMap
    val pocketData: Option[PocketData]

    val gameResult: GameResult
    val gameEnd: Boolean
    val gameOutcome: Int
    val gameScore: Int
    val p1Score: Int
    val p2Score: Int
    val legalMoves: Array[Int]
    val playerTurn: Int // 1 for South (P1/black) -1 for North (P2/white)
    def fenString: String
  }

  private class GoPosition(
      position: GoGame,
      ply: Int = 0,
      fromFen: Option[FEN] = None,
      komi: Int = 6
  ) extends Position {
    // TODO: yes, this is an abuse of scala. We could get an
    //       exception here, but I'm not sure how to work around that
    //       at the moment
    val variant = Variant.byKey("go19x19") // todo change for size?

    def makeMovesWithPrevious(movesList: List[Int], previousMoves: List[String]): Position = {

      var pos =
        if (previousMoves.length == 0 && Api.initialFen.value != fen.value) positionFromFen(fen.value)
        else if (Api.initialFen.value != initialFen.value) positionFromFen(initialFen.value)
        else new GoPosition(new GoGame(), 0, fromFen, komi)

      pos = pos.makeMoves(previousMoves.map(uciToMove))

      movesList.map { move =>
        if (pos.legalMoves.contains(move)) pos = pos.makeMoves(List(move))
        else
          sys.error(
            s"Illegal move1: ${move} from list: ${movesList} legalMoves: ${pos.legalMoves.map(_.toString()).mkString(", ")}"
          )
      }
      pos.setKomi(komi)
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
      return new GoPosition(position, ply + movesList.length, fromFen, komi)
    }

    // helper
    def toBoard: String = position.toBoard.toString

    def setBoard(goBoard: GoBoard): Unit = position.setBoard(goBoard)

    def setKomi(k: Int): Unit = position.setKomiScore(k)

    def goDiagram: String = position.toBoard.toDiagram

    def fenString: String = {
      val splitDiagram = goDiagram.split(' ')
      val board        = splitDiagram.lift(0).getOrElse(" board fen error ").replace("X", "S").replace("O", "s")
      val turn         =
        if (position.turn() == 1) "b"
        else "w" // cant trust engine fen - not sure why but it always returns 'b'
      val ko           = splitDiagram.lift(2).getOrElse("-").toString()
      val p1FinalScore = p1Score
      val p2FinalScore = p2Score
      val fullMoveStr  = (ply / 2 + 1).toString()
      val pocket       = "[SSSSSSSSSSssssssssss]"
      return s"${board}${pocket} ${turn} ${ko} ${p1FinalScore} ${p2FinalScore} ${komi} ${fullMoveStr}"
    }

    def toPosition = position.toBoard().position()

    lazy val fen: FEN = FEN(fenString)

    private def convertPieceMapFromFen(fenString: String): PieceMap = {
      val boardWidth            = variant.boardSize.width
      val boardFen              = fenString.split(' ').take(1).mkString("")
      val boardFenWithoutPocket = boardFen.split('[').take(1).mkString("")
      var pieces                = Map.empty[Pos, Piece]
      boardFenWithoutPocket
        .split('/')
        .zipWithIndex
        .map {
          case (row, rowIndex) => {
            var colIndex    = 0
            var isLastChar1 = false
            row.map(c => {
              c match {
                case 'X' | 'S' =>
                  Pos((boardWidth - rowIndex - 1) * boardWidth + colIndex).map { pos =>
                    {
                      pieces += (pos -> Piece(P1, Stone))
                      colIndex += 1
                    }
                  }
                case 'O' | 's' =>
                  Pos((boardWidth - rowIndex - 1) * boardWidth + colIndex).map { pos =>
                    {
                      pieces += (pos -> Piece(P2, Stone))
                      colIndex += 1
                    }
                  }
                case n         => {
                  colIndex += n.asDigit
                  if (isLastChar1) colIndex += 9
                }
                case _         => sys.error(s"unrecognaised character in Go fen, ${c}")
              }
              isLastChar1 = c == '1'
            })
          }
        }

      pieces
    }

    lazy val pieceMap: PieceMap = convertPieceMapFromFen(fenString)

    lazy val pocketData =
      Some(
        PocketData(
          Pockets(
            Pocket(List(strategygames.Role.GoRole(Stone), strategygames.Role.GoRole(Stone))),
            Pocket(List(strategygames.Role.GoRole(Stone), strategygames.Role.GoRole(Stone)))
          ),
          // Can make an empty Set of Pos because we dont have to track promoted pieces
          Set[Pos]()
        )
      )

    lazy val gameResult: GameResult =
      GameResult.resultFromInt(position.outcome(), position.hasEnded())

    lazy val gameEnd: Boolean = position.hasEnded()

    lazy val gameOutcome: Int = position.outcome()

    lazy val gameScore: Int = position.score() // black - (white + komi)

    lazy val p1Score: Int = position.blackScore() // black
    lazy val p2Score: Int = position.whiteScore() // white + komi

    val passMove: Int = 361

    val legalMoves: Array[Int] = {
      position.resetCursor()
      var moves: List[Int] = List()
      var nextMove         = position.nextMove()
      while (nextMove != -1) {
        moves = moves ::: List(nextMove)
        nextMove = position.nextMove()
      }
      moves.toArray.filter(m => m != passMove)
    }

    val playerTurn: Int = position.turn()

    val initialFen: FEN = fromFen.fold(Api.initialFen)(f => f)

  }

  def position: Position = {
    val g    = new GoGame()
    val komi = 6 // todo add as input from setup?
    g.setKomiScore(komi)
    new GoPosition(g)
  }

  // todo handle constants in go engine for different size boards and komi
  def positionFromVariant(variant: Variant): Position =
    variant.key match {
      case "go9x9"   => new GoPosition(new GoGame()) // todo setup 9x9
      case "go13x13" => new GoPosition(new GoGame()) // todo setup 13x13
      case "go19x19" => position
      case _         => new GoPosition(new GoGame())
    }

  def positionFromFen(fenString: String): Position = {
    val game = new GoGame()
    val fen  = FEN(fenString)
    game.setBoard(goBoardFromFen(fenString))
    game.setKomiScore(fen.komi)
    new GoPosition(game, fen.ply.getOrElse(0), Some(fen), fen.komi)
  }

  def positionFromVariantNameAndFEN(variantName: String, fenString: String): Position = {
    val game = new GoGame()
    val fen  = FEN(fenString)
    game.setBoard(goBoardFromFen(fenString))
    game.setKomiScore(fen.komi)
    variantName.toLowerCase() match {
      case "go19x19" => new GoPosition(game, fen.ply.getOrElse(0), Some(fen), fen.komi)
      case _         => new GoPosition(new GoGame(), fen.ply.getOrElse(0), Some(fen), fen.komi)
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

  def uciToMove(uciMove: String): Int = Pos.fromKey(uciMove.drop(2)).map(_.index).getOrElse(0)

  def moveToUci(move: Int): String = s"${Stone.forsyth.toUpper}@${Pos(move).map(_.key).getOrElse("a1")}"

  val initialFen: FEN = variant.Go19x19.initialFen

  val fenRegex                                = "([0-9Ss]?){1,19}(/([0-9Ss]?){1,19}){8,18}\\[[Ss]+\\] [w|b] - [0-9]+ [0-9]+ [0-9]+ [0-9]+"
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
