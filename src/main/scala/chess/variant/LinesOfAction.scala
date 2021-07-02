package chess
package variant

import chess.format.FEN

import scala.collection.immutable.Queue

case object LinesOfAction
    extends Variant(
      id = 11,
      key = "linesOfAction",
      name = "Lines Of Action",
      shortName = "LOA",
      title = "Connect all of your checkers to win.",
      standardInitialPosition = false
    ) {

  override val pieces: Map[Pos, Piece] = Map(
    Pos.B1 -> White.loachecker,
    Pos.C1 -> White.loachecker,
    Pos.D1 -> White.loachecker,
    Pos.E1 -> White.loachecker,
    Pos.F1 -> White.loachecker,
    Pos.G1 -> White.loachecker,
    Pos.B8 -> White.loachecker,
    Pos.C8 -> White.loachecker,
    Pos.D8 -> White.loachecker,
    Pos.E8 -> White.loachecker,
    Pos.F8 -> White.loachecker,
    Pos.G8 -> White.loachecker,
    Pos.A2 -> Black.loachecker,
    Pos.A3 -> Black.loachecker,
    Pos.A4 -> Black.loachecker,
    Pos.A5 -> Black.loachecker,
    Pos.A6 -> Black.loachecker,
    Pos.A7 -> Black.loachecker,
    Pos.H2 -> Black.loachecker,
    Pos.H3 -> Black.loachecker,
    Pos.H4 -> Black.loachecker,
    Pos.H5 -> Black.loachecker,
    Pos.H6 -> Black.loachecker,
    Pos.H7 -> Black.loachecker,
  )

  override val initialFen = FEN("1LLLLLL1/l6l/l6l/l6l/l6l/l6l/l6l/1LLLLLL1 w - - 0 1")
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
            linkedPieces + (pos),
            (nextPos.tail ++ neighboringColorPieces(color, pos, board)).distinct
          )
      }
    }

    firstPiece(color, board)
      .map(firstPiece => piecesGroupSize(
        Set(firstPiece),
        neighboringColorPieces(color, firstPiece, board)
      ) == numOfPieces(color, board))
      .getOrElse(false)
  }

  override def specialEnd(situation: Situation) =
    winForColor(Black, situation.board) ^ winForColor(White, situation.board)

  //this probably isnt done very nicely, is it correct to return None for a draw?
  override def winner(situation: Situation): Option[Color] = {
    val blackWin = winForColor(Black, situation.board)
    val whiteWin = winForColor(White, situation.board)
    if (blackWin && !whiteWin){
      Option(Black)
    } else if (!blackWin && whiteWin){
      Option(White)
    } else None
  }

  override def specialDraw(situation: Situation) =
    winForColor(Black, situation.board) && winForColor(White, situation.board)
}
