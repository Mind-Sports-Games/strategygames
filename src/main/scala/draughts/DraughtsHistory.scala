package strategygames.draughts

import strategygames.Player

import cats.syntax.option.none

import format.Uci
import variant.{ Standard, Variant }

// Consecutive king moves by the respective side.
case class KingMoves(
    p1: Int = 0,
    p2: Int = 0,
    p1King: Option[Pos] = None,
    p2King: Option[Pos] = None
) {

  def add(player: Player, pos: Option[Pos]) = copy(
    p1 = p1 + player.fold(1, 0),
    p2 = p2 + player.fold(0, 1),
    p1King = player.fold(pos, p1King),
    p2King = player.fold(p2King, pos)
  )

  def reset(player: Player) = copy(
    p1 = player.fold(0, p1),
    p2 = player.fold(p2, 0),
    p1King = player.fold(none, p1King),
    p2King = player.fold(p2King, none)
  )

  def nonEmpty = p1 > 0 || p2 > 0

  def apply(player: Player)   = player.fold(p1, p2)
  def kingPos(player: Player) = player.fold(p1King, p2King)
}

case class DraughtsHistory(
    //TODO When converting Draughts to Multiaction we need to change this
    lastMove: Option[Uci] = None,
    positionHashes: PositionHash = Hash.zero,
    kingMoves: KingMoves = KingMoves(),
    variant: Variant = Standard
) {

  /** Halfmove clock: This is the number of halfmoves since the last non-king move or capture. This is used to
    * determine if a draw can be claimed under the twentyfive-move rule.
    */
  def halfMoveClock = math.max(0, (positionHashes.length / Hash.size) - 1)

  // generates random positionHashes to satisfy the half move clock
  def setHalfMoveClock(v: Int) =
    copy(positionHashes = DraughtsHistory.spoofHashes(v + 1))

  /** Checks for threefold repetition, does not apply to frisian draughts
    */
  def threefoldRepetition: Boolean = !variant.frisianVariant && halfMoveClock >= 8 && {
    // compare only hashes for positions with the same side to move
    val positions = (positionHashes grouped Hash.size).sliding(1, 2).flatten.toList
    positions.headOption match {
      case Some(Array(x, y, z)) =>
        (positions count {
          case Array(x2, y2, z2) => x == x2 && y == y2 && z == z2
          case _                 => false
        }) >= 3
      case _                    => false
    }
  }

  def withLastMove(m: Uci) = copy(lastMove = Some(m))

  def withKingMove(player: Player, pos: Option[Pos], v: Boolean, resetOther: Boolean = false) =
    if (v && resetOther)
      copy(kingMoves = kingMoves.add(player, pos).reset(!player))
    else if (v)
      copy(kingMoves = kingMoves.add(player, pos))
    else if (resetOther)
      copy(kingMoves = KingMoves())
    else
      copy(kingMoves = kingMoves reset player)

  def withKingMoves(km: KingMoves) = copy(kingMoves = km)

  override def toString = {
    val positions = (positionHashes grouped Hash.size).toList
    s"${lastMove.fold("-")(_.uci)} ${positions.map(Hash.debug).mkString(" ")}"
  }
}

object DraughtsHistory {

  def make(
      lastMove: Option[String], // 0510 etc
      variant: Variant
  ): DraughtsHistory = DraughtsHistory(
    lastMove = lastMove flatMap Uci.apply,
    positionHashes = Array(),
    variant = variant
  )

  private def spoofHashes(n: Int): PositionHash = {
    (1 to n).toArray.flatMap { i =>
      Array((i >> 16).toByte, (i >> 8).toByte, i.toByte)
    }
  }
}
