package strategygames.backgammon

import strategygames.{ MultiPointState, Score }

import format.Uci

case class History(
    lastTurn: List[Uci] = List.empty,
    currentTurn: List[Uci] = List.empty,
    forcedTurn: Boolean = false,
    justUsedUndo: Boolean = false,
    positionHashes: PositionHash = Array.empty,
    score: Score = Score(0, 0),
    multiPointState: Option[MultiPointState] = None,
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

  def firstDiceRollHappened: Boolean =
    lastTurn.filter {
      case _: Uci.DiceRoll   => true
      case _: Uci.CubeAction => true
      case _                 => false
    }.nonEmpty

  def forcedTurnPersists(situation: Situation, action: Action) =
    if (currentTurn.size == 1) situation.forcedAction.map(_.toUci) == Some(action.toUci)
    else forcedTurn && situation.forcedAction.map(_.toUci) == Some(action.toUci)

}
