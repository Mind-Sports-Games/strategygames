package strategygames.chess.format

import strategygames.Player

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  def fullMove: Option[Int] = value.split(' ').lift(5).flatMap(_.toIntOption)

  // this only works for LOA because the fen uses w to mean p1 and b to mean p2
  def player: Option[Player] =
    value.split(' ').lift(1) flatMap (_.headOption) flatMap Player.apply

  def ply: Option[Int] =
    fullMove map { fm =>
      fm * 2 - (if (player.exists(_.p1)) 2 else 1)
    }

  def initial = value == Forsyth.initial.value
}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)
}
