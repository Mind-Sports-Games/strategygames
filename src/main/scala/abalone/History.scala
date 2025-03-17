package strategygames.abalone

import strategygames.{Player, Score}
import strategygames.abalone.format.Uci

case class History(
                    lastTurn: List[Uci] = List.empty,
                    currentTurn: List[Uci] = List.empty,
                    moves: List[(Player, Option[Move])] = List.empty,
                    positionHashes: PositionHash = Array.empty,
                    score: Score = Score(),
                    halfMoveClock: Int = 0
                  ) {
  lazy val lastAction: Option[Uci] = recentTurn.reverse.headOption

  lazy val recentTurn: List[Uci] = if (currentTurn.nonEmpty) currentTurn else lastTurn

  lazy val recentTurnUciString: Option[String] =
    if (recentTurn.nonEmpty) Some(recentTurn.map(_.uci).mkString(",")) else None

  private def isRepetition(times: Int) =
    positionHashes.length > (times - 1) * 4 * Hash.size && {
      //TODO Grand Abalone: we should only take hashes when the same player start their turn
      val positions = positionHashes.sliding(Hash.size, 2 * Hash.size).toList// compare only hashes for positions with the same side to move
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