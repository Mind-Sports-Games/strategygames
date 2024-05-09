package strategygames.backgammon

import strategygames.Score

import format.Uci

case class History(
    lastTurn: List[Uci] = List.empty,
    currentTurn: List[Uci] = List.empty,
    forcedTurn: Boolean = false,
    positionHashes: PositionHash = Array.empty,
    score: Score = Score(0, 0),
    // this is tracking fullMove for Backgammon
    halfMoveClock: Int = 0
) {

  lazy val lastAction: Option[Uci] =
    if (currentTurn.nonEmpty) currentTurn.reverse.headOption else lastTurn.reverse.headOption

  lazy val recentTurn: List[Uci] = if (currentTurn.nonEmpty) currentTurn else lastTurn

  lazy val recentTurnUciString: Option[String] =
    if (recentTurn.nonEmpty) Some(recentTurn.map(_.uci).mkString(",")) else None

  def hasRolledDiceThisTurn: Boolean =
    currentTurn.filter { case _: Uci.DiceRoll => true; case _ => false }.nonEmpty

  // can be used to determine whether this is the first turn or not
  def didRollDiceLastTurn: Boolean =
    lastTurn.filter { case _: Uci.DiceRoll => true; case _ => false }.nonEmpty

  def forcedTurnPersists(situation: Situation, action: Action) =
    if (currentTurn.size == 1) situation.forcedAction == Some(action)
    else forcedTurn && situation.forcedAction == Some(action)

}
