package strategygames.togyzkumalak

import strategygames.Score

import format.Uci

case class History(
    lastTurn: List[Uci] = List.empty,
    currentTurn: List[Uci] = List.empty,
    positionHashes: PositionHash = Array.empty,
    score: Score = Score(0, 0),
    // this might be tracking fullMove for Togyzkumalak
    halfMoveClock: Int = 0
) {

  lazy val lastAction: Option[Uci] =
    if (currentTurn.nonEmpty) currentTurn.reverse.headOption else lastTurn.reverse.headOption

  lazy val recentTurn: List[Uci] = if (currentTurn.nonEmpty) currentTurn else lastTurn

  lazy val recentTurnUciString: Option[String] =
    if (recentTurn.nonEmpty) Some(recentTurn.map(_.uci).mkString(",")) else None

}
