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
    Pos.B1 -> Black.queen,
    Pos.C1 -> Black.queen,
    Pos.D1 -> Black.queen,
    Pos.E1 -> Black.queen,
    Pos.F1 -> Black.queen,
    Pos.G1 -> Black.queen,
    Pos.B8 -> Black.queen,
    Pos.C8 -> Black.queen,
    Pos.D8 -> Black.queen,
    Pos.E8 -> Black.queen,
    Pos.F8 -> Black.queen,
    Pos.G8 -> Black.queen,
    Pos.A2 -> White.queen,
    Pos.A3 -> White.queen,
    Pos.A4 -> White.queen,
    Pos.A5 -> White.queen,
    Pos.A6 -> White.queen,
    Pos.A7 -> White.queen,
    Pos.H2 -> White.queen,
    Pos.H3 -> White.queen,
    Pos.H4 -> White.queen,
    Pos.H5 -> White.queen,
    Pos.H6 -> White.queen,
    Pos.H7 -> White.queen,
  )

  //picked queen to at least give us the option in early stages of being able to move pieces in all directions
  override val initialFen = FEN("1qqqqqq1/Q6Q/Q6Q/Q6Q/Q6Q/Q6Q/Q6Q/1qqqqqq1 b - - 0 1")

  override def allowsCastling = false

  override val castles = Castles.none

  override def valid(board: Board, strict: Boolean) =
    board.kingPos.isEmpty

  //copied from Atomic
  private def surroundingPositions(pos: Pos): Set[Pos] =
    Set(pos.up, pos.down, pos.left, pos.right, pos.upLeft, pos.upRight, pos.downLeft, pos.downRight).flatten

  private def neighboringColorPieces(color: Color, pos: Pos, situation: Situation) =
    situation.board.piecesOf(color).filterKeys(surroundingPositions(pos)).keySet.to(Queue)

  private def firstPiece(color: Color, situation: Situation): Option[Pos] =
    Option(situation.board.piecesOf(color).keySet.head)

  private def numOfPieces(color: Color, situation: Situation): Int =
    situation.board.piecesOf(color).size

  private def checkWinForColor(color: Color, situation: Situation): Boolean = {

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
            (nextPos.tail ++ neighboringColorPieces(color, pos, situation)).distinct
          )
      }
    }

    firstPiece(color, situation)
      .map(firstPiece => piecesGroupSize(
        Set(firstPiece),
        neighboringColorPieces(color, firstPiece, situation)
      ) == numOfPieces(color, situation))
      .getOrElse(false)
  }

  //havent tested specialEnd yet
  override def specialEnd(situation: Situation) =
    !List(White, Black).filter(checkWinForColor(_, situation)).isEmpty

  //this probably isnt done very nicely, and haven't considered draw yet
  override def winner(situation: Situation): Option[Color] = {
    val blackWin = checkWinForColor(Black, situation)
    val whiteWin = checkWinForColor(White, situation)
    if (blackWin && !whiteWin){
      Option(Black)
    } else if (!blackWin && whiteWin){
      Option(White)
    } else None
  }

}
