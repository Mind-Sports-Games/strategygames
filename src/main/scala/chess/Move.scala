package strategygames.chess
import strategygames.MoveMetrics

import strategygames.chess.format.Uci
import cats.syntax.option._
import scalalib.extensions.*

case class Move(
    piece: Piece,
    orig: Pos,
    dest: Pos,
    situationBefore: Situation,
    after: Board,
    autoEndTurn: Boolean,
    capture: Option[Pos],
    promotion: Option[PromotableRole],
    castle: Option[((Pos, Pos), (Pos, Pos))],
    enpassant: Boolean,
    metrics: MoveMetrics = MoveMetrics()
) extends Action(situationBefore) {

  def playerAfter = if (autoEndTurn) !piece.player else piece.player

  def situationAfter =
    Situation(finalizeAfter, playerAfter)

  def withHistory(h: History) = copy(after = after withHistory h)

  def finalizeAfter: Board = {
    val board = after updateHistory { h1 =>
      val h2 = h1.copy(
        // turn info is set when creating board for Move to enable calculations for multiaction
        // lastTurn = if (autoEndTurn) h1.currentTurn :+ toUci else h1.lastTurn,
        // currentTurn = if (autoEndTurn) List() else h1.currentTurn :+ toUci,
        unmovedRooks = before.unmovedRooks,
        halfMoveClock =
          if ((piece is Pawn) || captures || promotes) 0
          else h1.halfMoveClock + 1
      )

      // my broken castles
      if ((piece is King) && h2.canCastle(player).any)
        h2 withoutCastles player
      else if (piece is Rook) (for {
        kingPos <- after kingPosOf player
        side    <- Side.kingRookSide(kingPos, orig).filter { s =>
                     (h2 canCastle player on s) &&
                     h1.unmovedRooks.pos(orig)
                   }
      } yield h2.withoutCastle(player, side)) | h2
      else h2
    } fixCastles

    // Update position hashes last, only after updating the board,
    // castling rights and en-passant rights.
    board.variant.finalizeBoard(
      board,
      toUci,
      capture flatMap before.apply
    ) updateHistory { h =>
      lazy val positionHashesOfSituationBefore =
        if (h.positionHashes.isEmpty) Hash(situationBefore)
        else h.positionHashes
      val resetsPositionHashes                 = board.variant.isIrreversible(this)
      val basePositionHashes                   =
        if (resetsPositionHashes) Array.empty: PositionHash
        else positionHashesOfSituationBefore
      h.copy(positionHashes = Hash(Situation(board, !piece.player)) ++ basePositionHashes)
    }
  }

  def applyVariantEffect: Move = before.variant addVariantEffect this

  // does this move capture an opponent piece?
  def captures = capture.isDefined

  def promotes = promotion.isDefined

  def castles = castle.isDefined

  def normalizeCastle =
    castle.fold(this) { case (_, (rookOrig, _)) =>
      copy(dest = rookOrig)
    }

  def player = piece.player

  def withPromotion(op: Option[PromotableRole]): Option[Move] =
    op.fold(this.some) { p =>
      if ((after count Piece(player, Queen)) > (before count Piece(player, Queen))) for {
        b2 <- after take dest
        b3 <- b2.place(Piece(player, p), dest)
      } yield copy(after = b3, promotion = Option(p))
      else this.some
    }

  def withAfter(newBoard: Board) = copy(after = newBoard)

  def withMetrics(m: MoveMetrics): Move = copy(metrics = m)

  def toUci: Uci.Move = Uci.Move(orig, dest, promotion)

  override def toString = s"$piece ${toUci.uci}"
}
