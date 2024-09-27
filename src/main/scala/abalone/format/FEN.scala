package strategygames.abalone.format

import strategygames.Player
import strategygames.abalone.PieceMap

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  def player: Option[Player] =
    value.split(' ').lift(3) flatMap (_.headOption) flatMap Player.apply

  def player1Score: Int = intFromFen(1).getOrElse(0)

  def player2Score: Int = intFromFen(2).getOrElse(0)

  def fullMove: Option[Int] = intFromFen(4)

  def ply: Option[Int] =
    fullMove map { fm =>
      fm * 2 - (if (player.exists(_.p1)) 2 else 1)
    }

  private def intFromFen(index: Int): Option[Int] =
    value.split(' ').lift(index).flatMap(_.toIntOption)

  def initial = value == Forsyth.initial.value

  // TODO Abalone Set
  def pieces: PieceMap = Map.empty
}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)
}
