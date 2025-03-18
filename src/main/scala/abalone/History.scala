package strategygames.abalone

import strategygames.abalone.format.Uci
import strategygames.{Player, Score}

case class History(
                    lastTurn: List[Uci] = List.empty,
                    currentTurn: List[Uci] = List.empty,
                    prevPlayer: Option[Player] = None, // Contained in lastMove, but we may want to know the previous player without knowing the previous move
                    prevMove: Option[Move] = None,
                    positionHashes: PositionHash = Array.empty,
                    score: Score = Score(),
                    halfMoveClock: Int = 0
                  ) {
  lazy val lastAction: Option[Uci] = recentTurn.reverse.headOption

  lazy val recentTurn: List[Uci] = if (currentTurn.nonEmpty) currentTurn else lastTurn

  lazy val recentTurnUciString: Option[String] =
    if (recentTurn.nonEmpty) Some(recentTurn.map(_.uci).mkString(",")) else None

  private def isRepetition(times: Int): Boolean = {
    if (prevMove.isDefined && prevMove.get.autoEndTurn) {
      positionHashes.length > (times - 1) * 4 * Hash.size && {
        //val positions = positionHashes.sliding(Hash.size, 2 * Hash.size).toList
        val positions = positionHashes.sliding(size = Hash.size, step = Hash.size).toList // We could use step = 2*Hash.size in standard Abalone to compare only hashes for positions with the same side to move, but il would have to be different for Grand Abalone
        positions.headOption match {
          case Some(Array(x, y, z)) =>
            (positions count {
              case Array(x2, y2, z2) => x == x2 && y == y2 && z == z2
              case _ => false
            }) >= times
          case _ => times <= 1
        }
      }
    } else false
  }

  def threefoldRepetition = isRepetition(3)
}