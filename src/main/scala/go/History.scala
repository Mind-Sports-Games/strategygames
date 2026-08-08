package strategygames.go

import strategygames.Score

import format.Uci

/** What a go position remembers about how it got here.
  *
  * `positionHashes` is the house field every game logic uses for repetition, doing double duty as go's
  * positional-superko history: newest first, one entry per position the game has held. Go never truncates it
  * — a stone placed is never taken back — except at a settlement, where `Board.settled` restarts it.
  * `captures` is here because it is genuinely accumulated arithmetic; the area score is not, and lives on
  * `Board` (see `Board.areaScore`).
  */
case class History(
    lastTurn: List[Uci] = List.empty,
    currentTurn: List[Uci] = List.empty,
    positionHashes: PositionHash = Array.empty,
    halfMoveClock: Int = 0,
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
