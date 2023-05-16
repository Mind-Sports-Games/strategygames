package strategygames.go.format

import strategygames.Player

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  def player: Option[Player] =
    value.split(' ').lift(1) flatMap (_.headOption) flatMap Player.apply

  def player1Score: Int = intFromFen(3).getOrElse(0)

  def player2Score: Int = intFromFen(4).getOrElse(0)

  def fullMove: Option[Int] = intFromFen(5)

  def ply: Option[Int] =
    fullMove map { fm =>
      fm * 2 - (if (player.exists(_.p1)) 2 else 1)
    }

  def parts                 = value.split(' ')
  def board: Option[String] = parts.lift(0).flatMap(_.split('[').lift(0))
  def engineFen: String     = board.getOrElse("") ++ " " ++ (parts.lift(1) ++ parts.lift(2)).mkString(" ")

  private def intFromFen(index: Int): Option[Int] =
    value.split(' ').lift(index).flatMap(_.toIntOption)

  def initial = value == Forsyth.initial.value
}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)
}
