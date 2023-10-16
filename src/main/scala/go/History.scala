package strategygames.go

import strategygames.Score

import format.Uci

case class History(
    lastMove: Option[Uci] = None,
    positionHashes: PositionHash = Array.empty,
    halfMoveClock: Int = 0,
    score: Score = Score(0, 0),
    captures: Score = Score(0, 0)
)
