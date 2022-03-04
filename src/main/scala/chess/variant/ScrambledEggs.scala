package strategygames.chess.variant

import strategygames.chess._
import strategygames.chess.format.FEN
import strategygames.{ GameFamily, Player }

import scala.collection.immutable.Queue

case object ScrambledEggs
    extends Variant(
      id = 14,
      key = "scrambledEggs",
      name = "Scrambled Eggs",
      standardInitialPosition = false
    ) {

  def perfId: Int    = 22
  def perfIcon: Char = ''

  override def gameFamily: GameFamily = GameFamily.LinesOfAction()

  override def aiVariant: Boolean = false

  override val pieces: Map[Pos, Piece] = Map(
    Pos.B1 -> Piece(P1, LOAChecker),
    Pos.C1 -> Piece(P2, LOAChecker),
    Pos.D1 -> Piece(P1, LOAChecker),
    Pos.E1 -> Piece(P2, LOAChecker),
    Pos.F1 -> Piece(P1, LOAChecker),
    Pos.G1 -> Piece(P2, LOAChecker),
    Pos.B8 -> Piece(P2, LOAChecker),
    Pos.C8 -> Piece(P1, LOAChecker),
    Pos.D8 -> Piece(P2, LOAChecker),
    Pos.E8 -> Piece(P1, LOAChecker),
    Pos.F8 -> Piece(P2, LOAChecker),
    Pos.G8 -> Piece(P1, LOAChecker),
    Pos.A2 -> Piece(P2, LOAChecker),
    Pos.A3 -> Piece(P1, LOAChecker),
    Pos.A4 -> Piece(P2, LOAChecker),
    Pos.A5 -> Piece(P1, LOAChecker),
    Pos.A6 -> Piece(P2, LOAChecker),
    Pos.A7 -> Piece(P1, LOAChecker),
    Pos.H2 -> Piece(P1, LOAChecker),
    Pos.H3 -> Piece(P2, LOAChecker),
    Pos.H4 -> Piece(P1, LOAChecker),
    Pos.H5 -> Piece(P2, LOAChecker),
    Pos.H6 -> Piece(P1, LOAChecker),
    Pos.H7 -> Piece(P2, LOAChecker)
  )

  override val initialFen          = FEN("1lLlLlL1/L6l/l6L/L6l/l6L/L6l/l6L/1LlLlLl1 w - - 0 1")
  override def startPlayer: Player = P1

  override def allowsCastling = false

  override val castles = Castles.none

  override def valid(board: Board, strict: Boolean) =
    board.kingPos.isEmpty

  //copied from Atomic
  private def surroundingPositions(pos: Pos): Set[Pos] =
    Set(pos.up, pos.down, pos.left, pos.right, pos.upLeft, pos.upRight, pos.downLeft, pos.downRight).flatten

  private def neighboringPlayerPieces(player: Player, pos: Pos, board: Board): Queue[Pos] =
    board.piecesOf(player).keySet.filter(surroundingPositions(pos)).to(Queue)

  private def firstPiece(player: Player, board: Board): Option[Pos] =
    Option(board.piecesOf(player).keySet.head)

  private def numOfPieces(player: Player, board: Board): Int =
    board.piecesOf(player).size

  private def winForPlayer(player: Player, board: Board): Boolean = {

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
            (nextPos.tail ++ neighboringPlayerPieces(player, pos, board)).distinct
          )
      }
    }

    firstPiece(player, board)
      .map(firstPiece =>
        piecesGroupSize(
          Set(firstPiece),
          neighboringPlayerPieces(player, firstPiece, board)
        ) == numOfPieces(player, board)
      )
      .getOrElse(false)
  }

  override def specialEnd(situation: Situation) =
    winForPlayer(P2, situation.board) ^ winForPlayer(P1, situation.board)

  //this probably isnt done very nicely, is it correct to return None for a draw?
  override def winner(situation: Situation): Option[Player] = {
    val p2Win = winForPlayer(P2, situation.board)
    val p1Win = winForPlayer(P1, situation.board)
    if (p2Win && !p1Win) {
      Option(P2)
    } else if (!p2Win && p1Win) {
      Option(P1)
    } else None
  }

  override def specialDraw(situation: Situation) =
    winForPlayer(P2, situation.board) && winForPlayer(P1, situation.board)

}
