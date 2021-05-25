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
      title = "Lines of Action - Connect all of your pieces",
      standardInitialPosition = false
    ) {

  override val pieces: Map[Pos, Piece] = Map(
    Pos.B1 -> Black.loachecker,
    Pos.C1 -> Black.loachecker,
    Pos.D1 -> Black.loachecker,
    Pos.E1 -> Black.loachecker,
    Pos.F1 -> Black.loachecker,
    Pos.G1 -> Black.loachecker,
    Pos.B8 -> Black.loachecker,
    Pos.C8 -> Black.loachecker,
    Pos.D8 -> Black.loachecker,
    Pos.E8 -> Black.loachecker,
    Pos.F8 -> Black.loachecker,
    Pos.G8 -> Black.loachecker,
    Pos.A2 -> White.loachecker,
    Pos.A3 -> White.loachecker,
    Pos.A4 -> White.loachecker,
    Pos.A5 -> White.loachecker,
    Pos.A6 -> White.loachecker,
    Pos.A7 -> White.loachecker,
    Pos.H2 -> White.loachecker,
    Pos.H3 -> White.loachecker,
    Pos.H4 -> White.loachecker,
    Pos.H5 -> White.loachecker,
    Pos.H6 -> White.loachecker,
    Pos.H7 -> White.loachecker,
  )

  override val initialFen = FEN("1llllll1/L6L/L6L/L6L/L6L/L6L/L6L/1llllll1 b - - 0 1")
  override def startColor: Color = Black

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
    !List(White, Black).filter(winForColor(_, situation.board)).isEmpty

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
