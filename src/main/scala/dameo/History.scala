package strategygames.dameo

import format.Uci

import strategygames.Player

//TODO Dameo - might need to add extra info into history, like captured pieces on this turn
case class History(
    lastTurn: List[Uci] = List.empty,
    currentTurn: List[Uci] = List.empty,
    forcedTurn: Boolean = false,
    positionHashes: PositionHash = Array.empty,
    // not sure Dameo needs this? Might be only needed for Frisian?
    kingMoves: KingMoves = KingMoves(),
    // this is tracking fullMove for Dameo
    halfMoveClock: Int = 0
) {

  lazy val lastAction: Option[Uci] =
    if (currentTurn.nonEmpty) currentTurn.reverse.headOption else lastTurn.reverse.headOption

  lazy val recentTurn: List[Uci] = if (currentTurn.nonEmpty) currentTurn else lastTurn

  lazy val recentTurnUciString: Option[String] =
    if (recentTurn.nonEmpty) Some(recentTurn.map(_.uci).mkString(",")) else None

  private def isRepetition(times: Int) =
    positionHashes.length > (times - 1) * 4 * Hash.size && {
      // compare only hashes for positions with the same side to move
      val positions = positionHashes.sliding(Hash.size, 2 * Hash.size).toList
      positions.headOption match {
        case Some(Array(x, y, z)) =>
          (positions count {
            case Array(x2, y2, z2) => x == x2 && y == y2 && z == z2
            case _                 => false
          }) >= times
        case _                    => times <= 1
      }
    }

  def threefoldRepetition = isRepetition(3)

  // generates random positionHashes to satisfy the half move clock
  def setHalfMoveClock(v: Int) =
    copy(positionHashes = History.spoofHashes(v + 1))

}

object History {

  private def spoofHashes(n: Int): PositionHash = {
    (1 to n).toArray.flatMap { i =>
      Array((i >> 16).toByte, (i >> 8).toByte, i.toByte)
    }
  }

}

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
    p1King = player.fold(None, p1King),
    p2King = player.fold(p2King, None)
  )

  def nonEmpty = p1 > 0 || p2 > 0

  def apply(player: Player)   = player.fold(p1, p2)
  def kingPos(player: Player) = player.fold(p1King, p2King)
}
