package strategygames.go

import com.joansala.game.go.GoGame
import com.joansala.game.go.GoBoard

import cats.implicits._

import strategygames.{ Pocket, Pockets }
import strategygames.go.format.FEN
import strategygames.go.Pos
import strategygames.go.variant.Variant
import scala.util.Try

sealed abstract class GameResult extends Product with Serializable

object GameResult {
  final case class VariantEnd() extends GameResult
  final case class Draw()       extends GameResult
  final case class Ongoing()    extends GameResult

  def resultFromInt(value: Int, ended: Boolean, isRepetition: Boolean): GameResult =
    if (value.abs == 1000 && ended) GameResult.VariantEnd()
    else if (value == 0 && ended && isRepetition) GameResult.VariantEnd() // e.g. repeating 3 ko's
    else if (value == 0 && ended) GameResult.Draw()
    else if (!ended) GameResult.Ongoing()
    else sys.error(s"Unknown game result: ${value}")

}

object Api {

  abstract class Position {
    val variant: Variant

    // todo rename moves to actions to be consistent
    def makeMoves(movesList: List[String]): Position
    def makeMovesWithPrevious(
        movesList: List[String],
        previousMoves: List[String]
    ): Position

    def toBoard: String
    def goDiagram: String
    def setKomi(komi: Double): Unit
    def setBoard(goBoard: GoBoard): Unit

    val initialFen: FEN
    val fen: FEN
    val pieceMap: PieceMap
    val pocketData: Option[PocketData]

    val gameResult: GameResult
    val gameEnd: Boolean
    val gameOutcome: Int
    val isRepetition: Boolean
    val gameScore: Int
    val p1Score: Double
    val p2Score: Double
    val legalDrops: Array[Int]
    val legalActions: Array[Int]
    val playerTurn: Int // 1 for South (P1/black) -1 for North (P2/white)
    def fenString: String
  }

  private class GoPosition(
      position: GoGame,
      ply: Int = 0,
      fromFen: Option[FEN] = None,
      komi: Double = 6.5
  ) extends Position {

    val gameSize: Int    = position.toBoard().gameSize()
    val variant: Variant = gameSize match {
      case 9  => strategygames.go.variant.Go9x9
      case 13 => strategygames.go.variant.Go13x13
      case 19 => strategygames.go.variant.Go19x19
      case _  => sys.error("Incorrect game size from position")
    }

    def makeMovesWithPrevious(
        movesList: List[String],
        previousMoves: List[String]
    ): Position = {
      var pos =
        if (previousMoves.length == 0 && Api.initialFen(variant.key).value != fen.value)
          positionFromFen(fen.value) // keeping current position?
        else if (Api.initialFen(variant.key).value != initialFen.value)
          positionFromFen(initialFen.value)
        else new GoPosition(new GoGame(gameSize), 0, fromFen, komi)

      pos = pos.makeMoves(previousMoves)

      movesList.map { move =>
        {
          val engineMove: Int = uciToMove(move, variant)
          if (pos.legalActions.contains(engineMove)) pos = pos.makeMoves(List(move))
          else
            sys.error(
              s"Illegal move1: ${move} from list: ${movesList} legalActions: ${pos.legalActions.map(_.toString()).mkString(", ")}"
            )
        }
      }
      pos.setKomi(komi)
      return pos
    }

    def makeMoves(movesList: List[String]): Position = {
      movesList.map { move =>
        {
          val engineMove: Int   = uciToMove(move, variant)
          val isConsecutivePass = position.lastMove == passMove && move == "pass"
          if (isConsecutivePass) {
            position.unmakeMove() // allows for drops after two passes and disagreement on dead stones.
          } else if (move.take(3) == "ss:") {
            // update board and pass twice
            val deadStones: List[Pos] = move.drop(3).split(",").toList.flatMap(Pos.fromKey(_))
            if (deadStones.length > 0) {
              val fenWithoutDeadStones = FEN(removeDeadStones(deadStones, fenString, variant))
              position.setBoard(goBoardFromFen(fenWithoutDeadStones))
            }
            position.makeMove(engineMove)
            position.makeMove(engineMove)
          } else {
            if (position.legalMoves.contains(engineMove)) position.makeMove(engineMove)
            else
              sys.error(
                s"Illegal move2: ${engineMove} from list: ${movesList} legalMoves: ${position.legalMoves.map(_.toString()).mkString(", ")}"
              )
          }
        }
      }
      return new GoPosition(position, ply + movesList.size, fromFen, komi)
    }

    // helper
    def toBoard: String = position.toBoard.toString

    def setBoard(goBoard: GoBoard): Unit = position.setBoard(goBoard)

    def setKomi(k: Double): Unit = position.setKomiScore(k)

    def goDiagram: String = position.toBoard.toDiagram

    def fenString: String = {
      val splitDiagram = goDiagram.split(' ')
      val board        = splitDiagram.lift(0).getOrElse(" board fen error ").replace("X", "S").replace("O", "s")
      val turn         =
        if (position.turn() == 1) "b"
        else "w" // cant trust engine fen - not sure why but it always returns 'b'
      val ko          = splitDiagram.lift(2).getOrElse("-").toString()
      val p1FenScore  = (p1Score * 10).toInt
      val p2FenScore  = (p2Score * 10).toInt
      val fenKomi     = (komi * 10).toInt
      val fullMoveStr = (ply / 2 + 1).toString()
      val pocket      = "[SSSSSSSSSSssssssssss]"
      return s"${board}${pocket} ${turn} ${ko} ${p1FenScore} ${p2FenScore} 0 0 ${fenKomi} ${fullMoveStr}"
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
                case 'X' | 'S'      =>
                  moveToPos((boardWidth - rowIndex - 1) * boardWidth + colIndex, variant).map { pos =>
                    {
                      pieces += (pos -> Piece(P1, Stone))
                      colIndex += 1
                    }
                  }
                case 'O' | 's'      =>
                  moveToPos((boardWidth - rowIndex - 1) * boardWidth + colIndex, variant).map { pos =>
                    {
                      pieces += (pos -> Piece(P2, Stone))
                      colIndex += 1
                    }
                  }
                case n if n.isDigit => {
                  colIndex += n.asDigit
                  if (isLastChar1) colIndex += 9
                }
                case _              => sys.error(s"unrecognaised character in Go fen, ${c}")
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

    val passMove: Int = gameSize * gameSize

    lazy val isRepetition: Boolean = position.isRepetition() && (position.lastMove() != passMove)

    lazy val gameResult: GameResult =
      GameResult.resultFromInt(position.outcome(), position.hasEnded(), isRepetition)

    lazy val gameEnd: Boolean = position.hasEnded()

    lazy val gameOutcome: Int = position.outcome()

    lazy val gameScore: Int = position.score() // 10* (black - (white + komi))

    lazy val p1Score: Double = position.blackScore() // black
    lazy val p2Score: Double = position.whiteScore() // white + komi

    val legalActions: Array[Int] = {
      position.resetCursor()
      var moves: List[Int] = List()
      var nextMove         = position.nextMove()
      while (nextMove != -1) {
        moves = moves ::: List(nextMove)
        nextMove = position.nextMove()
      }
      moves.toArray
    }

    val legalDrops: Array[Int] = {
      legalActions.filter(m => m != passMove)
    }

    val playerTurn: Int = position.turn()

    val initialFen: FEN = fromFen.fold(Api.initialFen(variant.key))(f => f)

  }

  def position(variant: Variant, komi: Double = 6.5): Position = {
    val g = new GoGame(variant.boardSize.height)
    g.setKomiScore(komi)
    new GoPosition(g)
  }

  def positionFromVariant(variant: Variant): Position =
    variant.key match {
      case "go9x9" | "go13x13" | "go19x19" => position(variant)
      case _                               => sys.error(s"incorrect variant supplied ${variant}")
    }

  def positionFromFen(fenString: String): Position = {
    val positionFen = FEN(fenString)
    val game        = new GoGame(positionFen.gameSize)
    game.setBoard(goBoardFromFen(positionFen))
    game.setKomiScore(positionFen.komi)
    val ply         = positionFen.ply.getOrElse(1)
    new GoPosition(game, ply, Some(positionFen), positionFen.komi)
  }

  def positionFromVariantNameAndFEN(variantKey: String, fenString: String): Position = {
    val positionFen   = FEN(fenString)
    val gameSize: Int = (variantKey, positionFen.gameSize) match {
      case ("go9x9", 9)    => 9
      case ("go13x13", 13) => 13
      case ("go19x19", 19) => 19
      case _               => sys.error(s"incorrect variant name (${variantKey}) and/or fen (${positionFen})")
    }
    val game          = new GoGame(gameSize)
    game.setBoard(goBoardFromFen(positionFen))
    game.setKomiScore(positionFen.komi)

    val ply = positionFen.ply.getOrElse(1)
    new GoPosition(game, ply, Some(positionFen), positionFen.komi)
  }

  def goBoardFromFen(fen: FEN): GoBoard = {
    val b  = new GoBoard(fen.gameSize)
    val b2 = b.toBoard(fen.engineFen)
    b2
  }

  def positionFromVariantAndMoves(variant: Variant, uciMoves: List[String]): Position =
    positionFromVariant(variant).makeMoves(uciMoves)

  def positionFromStartingFenAndMoves(startingFen: FEN, uciMoves: List[String]): Position =
    positionFromFen(startingFen.value).makeMoves(uciMoves)

  def passMove(variant: Variant): Int = {
    val gameSize: Int = variant.boardSize.height
    gameSize * gameSize
  }

  def uciToMove(uciMove: String, variant: Variant): Int = {
    if (uciMove == "pass" || uciMove.take(3) == "ss:") passMove(variant)
    else {
      val gameSize: Int = variant.boardSize.height
      val dest          = uciMove.drop(2)

      val fileChar  = dest.charAt(0)
      val file: Int = File.fromChar(fileChar).map(_.index).getOrElse(0) // 0 index
      val rank: Int = dest.drop(1).toIntOption.getOrElse(0)             // 1 index

      gameSize * (rank - 1) + file
    }
  }

  def moveToUci(move: Int, variant: Variant): String = {
    if (move == passMove(variant)) "pass"
    else {
      val gameSize: Int = variant.boardSize.height
      val file: String  = File(move % gameSize).map(_.toString).getOrElse("a")
      val rank: Int     = (move / gameSize) + 1

      s"${Stone.forsyth.toUpper}@${file}${rank}"
    }
  }

  def moveToPos(move: Int, variant: Variant): Option[Pos] = {
    val gameSize: Int = variant.boardSize.height
    val file: String  = File(move % gameSize).map(_.toString).getOrElse("a")
    val rank: Int     = (move / gameSize) + 1
    Pos.fromKey(s"${file}${rank}")
  }

  def initialFen(variantKey: String): FEN = variantKey match {
    case "go9x9"   => variant.Go9x9.initialFen
    case "go13x13" => variant.Go13x13.initialFen
    case "go19x19" => variant.Go19x19.initialFen
    case _         => sys.error(s"not given a go variant name: ${variantKey}")
  }

  val fenRegex                                = "([0-9Ss]?){1,19}(/([0-9Ss]?){1,19}){8,18}\\[[Ss]+\\] [w|b] - [0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-9]+ [0-9]+"
  def validateFEN(fenString: String): Boolean =
    Try(goBoardFromFen(FEN(fenString))).isSuccess && fenString.matches(fenRegex)

  def pieceMapFromFen(variantKey: String, fenString: String): PieceMap = {
    positionFromVariantNameAndFEN(variantKey, fenString).pieceMap
  }

  def writeBoardFenFromPieceMap(pieceMap: PieceMap, variant: Variant): String = {
    val gameSize: Int      = variant.boardSize.height
    val gameRow: List[Int] = List.range(0, gameSize)
    val boardString        = gameRow.reverse
      .map(y =>
        gameRow
          .map(x => {
            val piece = moveToPos(y * gameSize + x, variant).flatMap(pieceMap.get(_))
            piece.fold("1") { p => if (p.player == P1) "S" else "s" }
          })
          .mkString("")
      )
      .mkString("/")

    "[1]{2,}".r.replaceAllIn(boardString, s => s.group(0).size.toString)
  }

  def removeDeadStones(deadStones: List[Pos], fenString: String, variant: Variant): String = {
    val pieceMap        = pieceMapFromFen(variant.key, fenString)
    val updatedPieceMap = pieceMap -- deadStones.toSet
    val boardString     = writeBoardFenFromPieceMap(updatedPieceMap, variant)

    val start = fenString.indexOf("[", 0)
    if (start > 0)
      boardString + fenString.substring(start, fenString.length)
    else boardString
  }

}
