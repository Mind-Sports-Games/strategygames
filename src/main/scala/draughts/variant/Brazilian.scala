package strategygames.draughts
package variant

import strategygames.Player

case object Brazilian
    extends Variant(
      id = 12,
      gameType = 26,
      key = "brazilian",
      name = "Brazilian",
      standardInitialPosition = false,
      boardSize = Board.D64
    ) {

  def perfId: Int = 123

  override def fenVariant    = true
  override def aiVariant     = false
  def pieces                 = Russian.pieces
  def initialFen             = Russian.initialFen
  def startingPosition       = Russian.startingPosition
  override val openingTables = List(OpeningTable.tableFMJDBrazilian, OpeningTable.tableIDFBasic)

  def captureDirs    = Standard.captureDirs
  def moveDirsPlayer = Standard.moveDirsPlayer
  def moveDirsAll    = Standard.moveDirsAll

  override def finalizeBoard(
      board: Board,
      uci: format.Uci.Move,
      captured: Option[List[Piece]],
      situationBefore: Situation,
      finalSquare: Boolean
  ): Board = {
    val remainingCaptures =
      if (finalSquare) 0 else situationBefore.captureLengthFrom(uci.orig).getOrElse(0) - 1
    if (remainingCaptures > 0) board
    else {
      val p1Actors = board.actorsOf(Player.P1)
      val p2Actors = board.actorsOf(Player.P2)
      val p1Kings  = p1Actors.count(_.piece is King)
      val p2Kings  = p2Actors.count(_.piece is King)
      val p1Pieces = p1Actors.size
      val p2Pieces = p2Actors.size
      def loneKing(strongPieces: Int, strongKings: Int, weakKing: Actor) =
        strongPieces == 3 && strongKings >= 1 && weakKing.onLongDiagonal && board.piecesOnLongDiagonal == 1
      val p1LoneKing =
        if (p1Kings == 1 && p1Pieces == 1 && p2Kings >= 1) {
          loneKing(p2Pieces, p2Kings, p1Actors.head)
        } else false
      val p2LoneKing =
        if (p2Kings == 1 && p2Pieces == 1 && p1Kings >= 1) {
          loneKing(p1Pieces, p1Kings, p2Actors.head)
        } else false
      if (p1LoneKing || p2LoneKing) {
        board updateHistory { h =>
          // "abuse" kingmove counter to count the amount of moves made on the long
          // diagonal by the side with a lone king against 3 (see 7.2.7)
          h.withKingMove(Player(p1LoneKing), None, true)
        } withoutGhosts
      } else board.withoutGhosts
    }
  }

  def maxDrawingMoves(board: Board): Option[Int] = Russian.maxDrawingMoves(board)
  def updatePositionHashes(
      board: Board,
      move: Move,
      hash: strategygames.draughts.PositionHash
  ): PositionHash =
    Russian.updatePositionHashes(board, move, hash)

  override def validSide(board: Board, strict: Boolean)(player: Player) = {
    val roles = board rolesOf player
    (roles.count(_ == Man) > 0 || roles.count(_ == King) > 0) &&
    (!strict || roles.size <= 12) &&
    (!menOnPromotionRank(board, player) || board.ghosts != 0)
  }
}
