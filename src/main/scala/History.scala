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
  )

  final case class Draughts(h: draughts.DraughtsHistory) extends History(
    lastMove = h.lastMove.map(Uci.wrap),
    positionHashes = h.positionHashes,
    variant = Some(Variant.Draughts(h.variant)),
    kingMoves = h.kingMoves
  )

  final case class FairySF(h: fairysf.History) extends History(
    lastMove = h.lastMove.map(Uci.wrap),
    positionHashes = h.positionHashes,
    halfMoveClock = h.halfMoveClock,
    variant = Some(Variant.FairySF(h.variant))
  )

  implicit def chessHistory(h: chess.History) = Chess(h)
  implicit def draughtsHistory(h: draughts.DraughtsHistory) = Draughts(h)
  implicit def fairysfHistory(h: fairysf.History) = FairySF(h)

  //lila
  def apply(
    lib: GameLogic,
    lastMove: Option[Uci] = None,
    positionHashes: PositionHash = Array.empty,
    variant: Option[Variant] = None,
    castles: chess.Castles = chess.Castles.all,
    checkCount: chess.CheckCount = chess.CheckCount(0, 0),
    unmovedRooks: chess.UnmovedRooks = chess.UnmovedRooks.default,
    kingMoves: draughts.KingMoves = draughts.KingMoves(),
    halfMoveClock: Int = 0
  ): History = lib match {
    case GameLogic.Draughts()
      => Draughts(draughts.DraughtsHistory(
        lastMove = lastMove.map(lm => lm.toDraughts),
        positionHashes = positionHashes,
        variant = variant match {
          case Some(Variant.Draughts(variant)) => variant
          case None => strategygames.draughts.variant.Standard
          case _ => sys.error("Mismatched variant types for draughts history")
        },
        kingMoves = kingMoves
      ))
    case GameLogic.Chess()
      => Chess(chess.History(
        lastMove = lastMove.map(lm => lm.toChess),
        positionHashes = positionHashes,
        castles = castles,
        checkCount = checkCount,
        unmovedRooks = unmovedRooks,
        halfMoveClock = halfMoveClock
      ))
    case GameLogic.FairySF()
      => FairySF(fairysf.History(
        lastMove = lastMove.map(lm => lm.toFairySF),
        positionHashes = positionHashes,
        halfMoveClock = halfMoveClock,
        variant = variant match {
          case Some(Variant.FairySF(variant)) => variant
          case None => strategygames.fairysf.variant.Shogi
          case _ => sys.error("Mismatched variant types for FairySF history")
        },
      ))
    case _ => sys.error("Mismatched gamelogic types 1")
  }

}
