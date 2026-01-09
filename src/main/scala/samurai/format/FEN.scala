package strategygames.samurai.format

import strategygames.Player

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  // def fullMove: Option[Int] = value.split(' ').lift(5).flatMap(_.toIntOption)

  def player: Option[Player] =
    value.split(' ').lift(FEN.playerIndex) flatMap (_.headOption) flatMap Player.apply

  def player1Score: Int = intFromFen(1).getOrElse(0)

  def player2Score: Int = intFromFen(2).getOrElse(0)

  def invertPlayer: Option[FEN] =
    // This is safe because player function ensures there is an element at playerIndex when doing split(' ')
    player.map { p => FEN(value.split(' ').updated(FEN.playerIndex, (!p).letter.toString).mkString(" ")) }

  def fullMove: Option[Int] = intFromFen(FEN.fullMoveIndex)

  def ply: Option[Int] =
    fullMove map { fm =>
      fm * 2 - (if (player.exists(_.p1)) 2 else 1)
    }

  private def intFromFen(index: Int): Option[Int] =
    value.split(' ').lift(index).flatMap(_.toIntOption)

  private def boardStr = value.split(' ')(0)

  def owareStoneArray: Array[Int] =
    (boardStr.split('/')(1).split(',') ++ boardStr.split('/')(0).split(',').reverse)
      .map(c =>
        c.toString() match {
          case x if 1 to 6 map (_.toString) contains x => Array.fill(x.toInt)(0)
          case _                                       => Array(c.dropRight(1).toInt)
        }
      )
      .flatten
      .toArray

  def stonesOwnedByPlayer(player: Player) = {
    val stonesByPlayer = owareStoneArray.splitAt(6)
    player.fold(stonesByPlayer._1, stonesByPlayer._2).sum
  }

  def initial = value == Forsyth.initial.value

  private def rawPlayerScore(player: Player) =
    player.fold(player1Score, player2Score) - stonesOwnedByPlayer(player)

  def finalStonesScored = player1Score + player2Score == 48

  def withoutFinalStonesScored =
    if (finalStonesScored)
      FEN(
        s"${boardStr} ${rawPlayerScore(Player.P1)} ${rawPlayerScore(Player.P2)} ${value.split(' ').drop(3).mkString(" ")}"
      )
    else this

}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)

  def playerIndex: Int = 3

  def fullMoveIndex: Int = 4

}
