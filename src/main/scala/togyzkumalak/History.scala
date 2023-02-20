package strategygames.togyzkumalak

import strategygames.Player

import format.Uci

//same template as chess.CheckCount
case class Score(p1: Int = 0, p2: Int = 0) {

  def add(player: Player) =
    copy(
      p1 = p1 + player.fold(1, 0),
      p2 = p2 + player.fold(0, 1)
    )

  def nonEmpty = p1 > 0 || p2 > 0

  def apply(player: Player) = player.fold(p1, p2)

  def fenStr = s"${p1} ${p2}"

}

case class History(
    lastMove: Option[Uci] = None,
    positionHashes: PositionHash = Array.empty,
    score: Score = Score(0, 0),
    // this might be tracking fullMove for Togyzkumalak
    halfMoveClock: Int = 0
)
