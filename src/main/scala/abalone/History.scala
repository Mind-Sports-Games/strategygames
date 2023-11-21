package strategygames.abalone

import strategygames.Score

import format.Uci

case class History(
    lastMove: Option[Uci] = None,
    positionHashes: PositionHash = Array.empty,
    score: Score = Score(0, 0),
    // this might be tracking fullMove for Abalone
    halfMoveClock: Int = 0
)
