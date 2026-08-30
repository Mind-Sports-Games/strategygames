package strategygames.go

import strategygames.Score

import format.Uci

// NOTE: go enforces positional superko, so `positionHashes` accumulates every position the game has
// reached, and `hasOccurred` scans the whole run on every capturing placement.
case class History(
    lastTurn: List[Uci] = List.empty,
    currentTurn: List[Uci] = List.empty,
    positionHashes: PositionHash = Array.empty,
    halfMoveClock: Int = 0,
    // NOTE: `Board.areaScore` derives the score from the stones, and that is what
    // `strategygames.History.score` reports for a go game, so a caller that sets this field reads the
    // position's own score back. It survives because `readGoGame` names it.
    // TODO(lila): stop passing a score for go, and this field can be deleted along with `StoredPosition`.
    score: Score = Score(0, 0),
    captures: Score = Score(0, 0)
) {

  lazy val lastAction: Option[Uci] =
    if (currentTurn.nonEmpty) currentTurn.reverse.headOption else lastTurn.reverse.headOption

  lazy val recentTurn: List[Uci] = if (currentTurn.nonEmpty) currentTurn else lastTurn

  lazy val recentTurnUciString: Option[String] =
    if (recentTurn.nonEmpty) Some(recentTurn.map(_.uci).mkString(",")) else None

  def positionCount: Int = positionHashes.length / Hash.size

  def positionAt(index: Int): Long = Hash.hashAt(positionHashes, index)

  def currentPosition: Option[Long] = if (positionCount > 0) Some(positionAt(0)) else None

  def hasOccurred(hash: Long): Boolean = (0 until positionCount).exists(positionAt(_) == hash)

  def afterPosition(hash: Long): History =
    copy(positionHashes = Hash.bytesOf(hash) ++ positionHashes)

  def startingAtPosition(hash: Long): History =
    copy(positionHashes = Hash.bytesOf(hash))

}
