package strategygames.chess.variant

import strategygames.chess._
import strategygames.chess.format.FEN
import strategygames.{ Color, GameFamily }

import scala.collection.immutable.Queue

case object ScrambledEggs
    extends Variant(
      id = 14,
      key = "scrambledEggs",
      name = "Scrambled Eggs",
      shortName = "seggs",
      title = "Connect all of your scrambled checkers to win.",
      standardInitialPosition = false
    ) {

  def perfId: Int    = 22
  def perfIcon: Char = ''

  override def gameFamily: GameFamily = GameFamily.LinesOfAction()

  override def aiVariant: Boolean   = false

  override val pieces: Map[Pos, Piece] = Map(
    Pos.B1 -> Piece(White, LOAChecker),
    Pos.C1 -> Piece(Black, LOAChecker),
    Pos.D1 -> Piece(White, LOAChecker),
    Pos.E1 -> Piece(Black, LOAChecker),
    Pos.F1 -> Piece(White, LOAChecker),
    Pos.G1 -> Piece(Black, LOAChecker),
    Pos.B8 -> Piece(Black, LOAChecker),
    Pos.C8 -> Piece(White, LOAChecker),
    Pos.D8 -> Piece(Black, LOAChecker),
    Pos.E8 -> Piece(White, LOAChecker),
    Pos.F8 -> Piece(Black, LOAChecker),
    Pos.G8 -> Piece(White, LOAChecker),
    Pos.A2 -> Piece(Black, LOAChecker),
    Pos.A3 -> Piece(White, LOAChecker),
    Pos.A4 -> Piece(Black, LOAChecker),
    Pos.A5 -> Piece(White, LOAChecker),
    Pos.A6 -> Piece(Black, LOAChecker),
    Pos.A7 -> Piece(White, LOAChecker),
    Pos.H2 -> Piece(White, LOAChecker),
    Pos.H3 -> Piece(Black, LOAChecker),
    Pos.H4 -> Piece(White, LOAChecker),
    Pos.H5 -> Piece(Black, LOAChecker),
    Pos.H6 -> Piece(White, LOAChecker),
    Pos.H7 -> Piece(Black, LOAChecker)
  )

  override val initialFen        = FEN("1lLlLlL1/L6l/l6L/L6l/l6L/L6l/l6L/1LlLlLl1 w - - 0 1")
  override def startColor: Color = White

  override def allowsCastling = false

  override val castles = Castles.none

  override def valid(board: Board, strict: Boolean) =
    board.kingPos.isEmpty

  //copied from Atomic
  private def surroundingPositions(pos: Pos): Set[Pos] =
    Set(pos.up, pos.down, pos.left, pos.right, pos.upLeft, pos.upRight, pos.downLeft, pos.downRight).flatten

  private def neighboringColorPieces(color: Color, pos: Pos, board: Board): Queue[Pos] =
    board.piecesOf(color).keySet.filter(surroundingPositions(pos)).to(Queue)

  private def firstPiece(color: Color, board: Board): Option[Pos] =
    Option(board.piecesOf(color).keySet.head)

  private def numOfPieces(color: Color, board: Board): Int =
    board.piecesOf(color).size

  private def winForColor(color: Color, board: Board): Boolean = {

    def piecesGroupSize(
        linkedPieces: Set[Pos],
        nextPos: Queue[Pos]
    ): Int = {
      if (nextPos.size == 0)
        linkedPieces.size
      else {
        val pos = nextPos.head
        if (linkedPieces.contains(pos))
          piecesGroupSize(
            linkedPieces,
            nextPos.tail
          )
        else
          piecesGroupSize(
            linkedPieces + pos,
            (nextPos.tail ++ neighboringColorPieces(color, pos, board)).distinct
          )
      }
    }

    firstPiece(color, board)
      .map(firstPiece =>
        piecesGroupSize(
          Set(firstPiece),
          neighboringColorPieces(color, firstPiece, board)
        ) == numOfPieces(color, board)
      )
      .getOrElse(false)
  }

  override def specialEnd(situation: Situation) =
    winForColor(Black, situation.board) ^ winForColor(White, situation.board)

  //this probably isnt done very nicely, is it correct to return None for a draw?
  override def winner(situation: Situation): Option[Color] = {
    val blackWin = winForColor(Black, situation.board)
    val whiteWin = winForColor(White, situation.board)
    if (blackWin && !whiteWin) {
      Option(Black)
    } else if (!blackWin && whiteWin) {
      Option(White)
    } else None
  }

  override def specialDraw(situation: Situation) =
    winForColor(Black, situation.board) && winForColor(White, situation.board)

}
