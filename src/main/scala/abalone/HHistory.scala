package abalone

import abalone.format.UUci
import strategygames.Score
import strategygames.abalone.{Hash, PositionHash}

case class HHistory(
                     lastTurn: List[UUci] = List.empty,
                     currentTurn: List[UUci] = List.empty,
                     positionHashes: PositionHash = Array.empty,
                     score: Score = Score(0, 0),
                     halfMoveClock: Int = 0
                   ) {

  lazy val lastAction: Option[UUci] =
    if (currentTurn.nonEmpty) currentTurn.reverse.headOption else lastTurn.reverse.headOption

  lazy val recentTurn: List[UUci] = if (currentTurn.nonEmpty) currentTurn else lastTurn

  lazy val recentTurnUciString: Option[String] =
    if (recentTurn.nonEmpty) Some(recentTurn.map(_.uci).mkString(",")) else None

  private def isRepetition(times: Int) =
    positionHashes.length > (times - 1) * 4 * Hash.size && {
      // compare only hashes for positions with the same side to move
      val positions = positionHashes.sliding(Hash.size, 2 * Hash.size).toList
      positions.headOption match {
        case Some(Array(x, y, z)) =>
          (positions count {
            case Array(x2, y2, z2) => x == x2 && y == y2 && z == z2
            case _ => false
          }) >= times
        case _ => times <= 1
      }
    }

  def threefoldRepetition = isRepetition(3)
}
