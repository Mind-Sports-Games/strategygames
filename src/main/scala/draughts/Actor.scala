package strategygames.draughts

import strategygames.Player

import scala.annotation.tailrec

case class Actor(
    piece: Piece,
    pos: PosMotion,
    board: Board
) {

  lazy val noncaptures: List[Move]   = noncaptureMoves()
  lazy val captures: List[Move]      = captureMoves(false)
  lazy val capturesFinal: List[Move] = captureMoves(true)

  lazy val captureLength = captures.foldLeft(0) { case (max, move) =>
    move.capture.fold(max) { jumps =>
      if (jumps.length > max) jumps.length
      else max
    }
  }

  def getCaptures(finalSquare: Boolean) = if (finalSquare) capturesFinal else captures

  private def noncaptureMoves(): List[Move] = piece.role match {
    case Man  => shortRangeMoves(board.variant.moveDirsPlayer(player), true)
    case King =>
      if (
        board.variant.frisianVariant && board.history
          .kingMoves(player) >= 3 && board.history.kingMoves.kingPos(player).fold(true)(_ == pos)
      ) Nil
      else if (!board.variant.flyingKings)
        shortRangeMoves(board.variant.moveDirsAll, false)
      else longRangeMoves(board.variant.moveDirsAll)
    case _    => Nil
  }

  private def captureMoves(finalSquare: Boolean): List[Move] = piece.role match {
    case Man  => board.variant.shortRangeCaptures(this, finalSquare)
    case King => board.variant.longRangeCaptures(this, finalSquare)
    case _    => Nil
  }

  def player: Player         = piece.player
  def is(c: Player): Boolean = c == piece.player
  def is(p: Piece): Boolean  = p == piece

  def onLongDiagonal: Boolean =
    (pos.x + ((pos.y + 1) / 2 - 1)) % (board.boardSize.width / 2) == 0

  private def shortRangeMoves(dirs: Directions, checkPromotion: Boolean): List[Move] =
    dirs flatMap { _._2(pos) } flatMap { to =>
      board.pieces.get(to) match {
        case None if checkPromotion  =>
          board.move(pos, to) map { move(to, _, true, None, None) } flatMap board.variant.maybePromote
        case None if !checkPromotion =>
          board.move(pos, to) map { move(to, _, true, None, None) }
        case Some(_)                 => Nil
      }
    }

  private def longRangeMoves(dirs: Directions): List[Move] = {
    val buf = new scala.collection.mutable.ArrayBuffer[Move]

    @tailrec
    def addAll(p: PosMotion, dir: Direction): Unit = {
      dir._2(p) match {
        case Some(to) =>
          board.pieces.get(to) match {
            case None =>
              board.move(pos, to).foreach { buf += move(to, _, true, None, None) }
              addAll(to, dir)
            case _    => // occupied
          }
        case _        => // past end of board
      }
    }

    dirs foreach { addAll(pos, _) }
    buf.toList
  }

  def move(
      dest: Pos,
      after: Board,
      /* Set this to upgrade to multimove */
      autoEndTurn: Boolean,
      /* Single capture or none */
      capture: Option[Pos],
      taken: Option[Pos]
  ) = Move(
    piece = piece,
    orig = pos,
    dest = dest,
    situationBefore = Situation(board, piece.player),
    after = after,
    autoEndTurn = autoEndTurn,
    capture = capture.map(List(_)),
    taken = taken.map(List(_))
  )

  def move(
      /** Destination square of the move */
      dest: Pos,
      /** Board after this move is made */
      after: Board,
      /** Chained captures (1x2x3) */
      capture: List[Pos],
      /** Pieces taken from the board */
      taken: List[Pos],
      promotion: Option[PromotableRole] = None,
      /** TODO Set this to upgrade to multimove */
      autoEndTurn: Boolean = true
  ) = Move(
    piece = piece,
    orig = pos,
    dest = dest,
    situationBefore = Situation(board, piece.player),
    after = after,
    autoEndTurn = autoEndTurn,
    capture = Some(capture),
    taken = Some(taken),
    promotion = promotion
  )
}
