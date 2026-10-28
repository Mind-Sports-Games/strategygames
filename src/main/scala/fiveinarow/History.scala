package strategygames.fiveinarow

import strategygames.{ Player, Score }

import format.Uci

case class History(
    lastTurn: List[Uci] = List.empty,
    currentTurn: List[Uci] = List.empty,
    positionHashes: PositionHash = Array.empty,
    blackSeat: Player = Player.P1,
    openingStep: OpeningStep = OpeningStep.Opening,
    score: Score = Score(0, 0),
    offered: List[Pos] = List.empty,
    forbiddenDrop: Option[Pos] = None,
    halfMoveClock: Int = 0
) {

  def setHalfMoveClock(v: Int) = copy(halfMoveClock = v)

  def record(uci: Uci, endsTurn: Boolean): History =
    if (endsTurn) copy(lastTurn = currentTurn :+ uci, currentTurn = List.empty)
    else copy(currentTurn = currentTurn :+ uci)

  lazy val lastAction: Option[Uci] =
    if (currentTurn.nonEmpty) currentTurn.lastOption else lastTurn.lastOption

  lazy val recentTurn: List[Uci] = if (currentTurn.nonEmpty) currentTurn else lastTurn

  lazy val recentTurnUciString: Option[String] =
    if (recentTurn.nonEmpty) Some(recentTurn.map(_.uci).mkString(",")) else None

}
