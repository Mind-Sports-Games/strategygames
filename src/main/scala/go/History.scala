package strategygames.go

import strategygames.Score

import format.Uci

// NOTE: go enforces positional superko, so `positionHashes` accumulates every position the game has
// reached, and `hasOccurred` scans the whole run on every capturing placement.
//
// NOTE: `score` is the position's area score rather than an accumulated total, and it is what
// `strategygames.History.Go` reports for a go game. `Board` re-points it at itself on every
// transition that can move a stone (see `Board.rescored`), so it cannot go stale, and it is taken by
// name so that the flood fill behind it runs only for a caller that reads the number. That by-name
// parameter is the reason this is a plain class rather than a `case class` — a case class parameter
// may not be by-name. `apply`, `copy` and every accessor keep the signatures the case class had.
final class History(
    val lastTurn: List[Uci] = List.empty,
    val currentTurn: List[Uci] = List.empty,
    val positionHashes: PositionHash = Array.empty,
    val halfMoveClock: Int = 0,
    scoreOfPosition: => Score = Score(0, 0),
    val captures: Score = Score(0, 0)
) {

  lazy val score: Score = scoreOfPosition

  def copy(
      lastTurn: List[Uci] = lastTurn,
      currentTurn: List[Uci] = currentTurn,
      positionHashes: PositionHash = positionHashes,
      halfMoveClock: Int = halfMoveClock,
      score: => Score = scoreOfPosition,
      captures: Score = captures
  ): History =
    new History(lastTurn, currentTurn, positionHashes, halfMoveClock, score, captures)

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

  // NOTE: `score` is derived from the board, so it is not part of what makes two histories equal, and
  // reading it here would run a flood fill per comparison. `positionHashes` compares by reference, as
  // it did while this was a case class holding an array.
  override def equals(that: Any): Boolean = that match {
    case h: History =>
      lastTurn == h.lastTurn &&
      currentTurn == h.currentTurn &&
      (positionHashes eq h.positionHashes) &&
      halfMoveClock == h.halfMoveClock &&
      captures == h.captures
    case _          => false
  }

  override def hashCode: Int = (lastTurn, currentTurn, halfMoveClock, captures).hashCode

  override def toString: String =
    s"History(${lastTurn}, ${currentTurn}, ${halfMoveClock}, ${captures})"

}

object History {

  def apply(
      lastTurn: List[Uci] = List.empty,
      currentTurn: List[Uci] = List.empty,
      positionHashes: PositionHash = Array.empty,
      halfMoveClock: Int = 0,
      score: => Score = Score(0, 0),
      captures: Score = Score(0, 0)
  ): History =
    new History(lastTurn, currentTurn, positionHashes, halfMoveClock, score, captures)

}
