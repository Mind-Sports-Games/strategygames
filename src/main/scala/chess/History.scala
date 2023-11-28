package strategygames.chess

import strategygames.Player

import format.Uci

// Checks received by the respective side.
case class CheckCount(p1: Int = 0, p2: Int = 0) {

  def add(player: Player) =
    copy(
      p1 = p1 + player.fold(1, 0),
      p2 = p2 + player.fold(0, 1)
    )

  def nonEmpty = p1 > 0 || p2 > 0

  def apply(player: Player) = player.fold(p1, p2)
}

case class UnmovedRooks(pos: Set[Pos]) extends AnyVal

object UnmovedRooks {
  val default = UnmovedRooks((Pos.p1Backrank ::: Pos.p2Backrank).toSet)
}

case class History(
    lastTurn: List[Uci] = List.empty,
    currentTurn: List[Uci] = List.empty,
    positionHashes: PositionHash = Array.empty,
    castles: Castles = Castles.all,
    checkCount: CheckCount = CheckCount(0, 0),
    unmovedRooks: UnmovedRooks = UnmovedRooks.default,
    halfMoveClock: Int = 0
) {
  def setHalfMoveClock(v: Int) = copy(halfMoveClock = v)

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

  def fivefoldRepetition = isRepetition(5)

  def canCastle(player: Player) = castles can player

  lazy val lastAction: Option[Uci] =
    if (currentTurn.nonEmpty) currentTurn.reverse.headOption
    else lastTurn.reverse.headOption

  lazy val recentTurn: List[Uci] =
    if (currentTurn.nonEmpty) currentTurn else lastTurn

  lazy val recentTurnUciString: Option[String] =
    if (recentTurn.nonEmpty) Some(recentTurn.map(_.uci).mkString(",")) else None

  def withoutCastles(player: Player) = copy(castles = castles without player)

  def withoutAnyCastles = copy(castles = Castles.none)

  def withoutCastle(player: Player, side: Side) =
    copy(castles = castles.without(player, side))

  def withCastles(c: Castles) = copy(castles = c)

  def withCheck(player: Player, v: Boolean) =
    if (v) copy(checkCount = checkCount add player) else this

  def withCheckCount(cc: CheckCount) = copy(checkCount = cc)

  override def toString = {
    val positions = (positionHashes grouped Hash.size).toList
    s"${recentTurnUciString.getOrElse("-")} ${positions.map(Hash.debug).mkString(" ")}"
  }
}

object History {

  def castle(player: Player, kingSide: Boolean, queenSide: Boolean) =
    History(
      castles = player match {
        case P1 =>
          Castles.init.copy(
            p1KingSide = kingSide,
            p1QueenSide = queenSide
          )
        case P2 =>
          Castles.init.copy(
            p2KingSide = kingSide,
            p2QueenSide = queenSide
          )
      }
    )

  def noCastle = History(castles = Castles.none)
}
