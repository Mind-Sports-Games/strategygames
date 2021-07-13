package strategygames

import format.Uci
import variant.Variant

sealed class History(
  val lastMove: Option[Uci] = None,
  val positionHashes: PositionHash = Array.empty,
  val variant: Option[Variant] = None,
  val castles: chess.Castles = chess.Castles.all,
  val checkCount: chess.CheckCount = chess.CheckCount(0, 0),
  val unmovedRooks: chess.UnmovedRooks = chess.UnmovedRooks.default,
  val kingMoves: draughts.KingMoves = draughts.KingMoves(),
  val halfMoveClock: Int = 0
) {

  def setHalfMoveClock(v: Int): History

  def threefoldRepetition: Boolean

  def withLastMove(m: Uci): History

  override def toString = {
    val positions = (positionHashes grouped Hash.size).toList
    s"${lastMove.fold("-")(_.uci)} ${positions.map(Hash.debug).mkString(" ")}"
  }

}

object History {

  final case class Chess(h: chess.History) extends History(
    lastMove = h.lastMove,
    positionHashes = h.positionHashes,
    castles = h.castles,
    checkCount = h.checkCount,
    unmovedRooks = h.unmovedRooks,
    halfMoveClock = h.halfMoveClock
  ) {

    def setHalfMoveClock(v: Int): History = Chess(h.setHalfMoveClock(v))

    def threefoldRepetition: Boolean = h.threefoldRepetition

    def withLastMove(m: Uci): History = Chess(h.withLastMove(m))

  }

  final case class Draughts(h: draughts.DraughtsHistory) extends History(
    lastMove = h.lastMove,
    positionHashes = h.positionHashes,
    variant = h.Variant.some,
    kingMoves = h.kingMoves
  ) {

    def setHalfMoveClock(v: Int): History = Draughts(h.setHalfMoveClock(v))

    def threefoldRepetition: Boolean = h.threefoldRepetition

    def withLastMove(m: Uci): History = Draughts(h.withLastMove(m))

  }

}
