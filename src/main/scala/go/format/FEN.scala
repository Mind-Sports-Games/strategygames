package strategygames.go.format

import strategygames.Player

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  def player: Option[Player] =
    value.split(' ').lift(1) flatMap (_.headOption) flatMap Player.apply

  def player1Score: Int = intFromFen(3).getOrElse(0)

  def player2Score: Int = intFromFen(4).getOrElse(0)

  def fullMove: Option[Int] = intFromFen(6)

  def ply: Option[Int] =
    fullMove map { fm =>
      fm * 2 - (if (player.exists(_.p1)) 2 else 1)
    }

  def komi: Int = intFromFen(5).getOrElse(0)

  def engineFen: String =
    removePockets(value.split(' ').take(3).mkString(" ")).replace("S", "X").replace("s", "O")

  private def removePockets(fen: String): String = {
    val start = fen.indexOf("[", 0)
    val end   = fen.indexOf("]", start)
    if (start > 0 && end > 0)
      fen.substring(0, start) + fen.substring(end + 1, fen.length)
    else fen
  }

  private def intFromFen(index: Int): Option[Int] =
    value.split(' ').lift(index).flatMap(_.toIntOption)

  def initial = value == Forsyth.initial.value
}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)
}
