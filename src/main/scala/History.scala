package strategygames

import format.Uci
import variant.Variant

abstract sealed class History(
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
    lastMove = h.lastMove.map(Uci.wrap),
    positionHashes = h.positionHashes,
    castles = h.castles,
    checkCount = h.checkCount,
    unmovedRooks = h.unmovedRooks,
    halfMoveClock = h.halfMoveClock
  ) {

    def setHalfMoveClock(v: Int): History = Chess(h.setHalfMoveClock(v))

    def threefoldRepetition: Boolean = h.threefoldRepetition

    def withLastMove(m: Uci): History = m match {
      case u: Uci.Chess    => Chess(h.withLastMove(u.unwrap))
      case _ => sys.error("Not passed Chess objects")
    }

  }

  final case class Draughts(h: draughts.DraughtsHistory) extends History(
    lastMove = h.lastMove.map(Uci.wrap),
    positionHashes = h.positionHashes,
    variant = Some(Variant.Draughts(h.variant)),
    kingMoves = h.kingMoves
  ) {

    def setHalfMoveClock(v: Int): History = Draughts(h.setHalfMoveClock(v))

    def threefoldRepetition: Boolean = h.threefoldRepetition

    def withLastMove(m: Uci): History = m match {
      case u: Uci.Draughts => Draughts(h.withLastMove(u.unwrap))
      case _ => sys.error("Not passed Draughts objects")
    }

  }

  implicit def chessHistory(h: chess.History) = Chess(h)
  implicit def draughtsHistory(h: draughts.DraughtsHistory) = Draughts(h)

  def apply(
    lib: GameLib,
    lastMove: Option[Uci] = None,
    positionHashes: PositionHash = Array.empty,
    variant: Option[Variant] = None,
    castles: chess.Castles = chess.Castles.all,
    checkCount: chess.CheckCount = chess.CheckCount(0, 0),
    unmovedRooks: chess.UnmovedRooks = chess.UnmovedRooks.default,
    kingMoves: draughts.KingMoves = draughts.KingMoves(),
    halfMoveClock: Int = 0
  ): History = (lib, lastMove, variant) match {
    case (GameLib.Draughts(), Some(lastMove: Uci.Draughts), Some(Variant.Draughts(variant)))
      => Draughts(draughts.DraughtsHistory(
        lastMove = Some(lastMove.unwrap),
        positionHashes = positionHashes,
        variant = variant,
        kingMoves = kingMoves
      ))
    case (GameLib.Chess(), Some(lastMove: Uci.Chess), Some(Variant.Chess(_)))
      => Chess(chess.History(
        lastMove = Some(lastMove.unwrap),
        positionHashes = positionHashes,
        castles = castles,
        checkCount = checkCount,
        unmovedRooks = unmovedRooks,
        halfMoveClock = halfMoveClock
      ))
    case _ => sys.error("Mismatched gamelib types")
  }

}
