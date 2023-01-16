package strategygames.mancala.format

import strategygames.Player

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  // def fullMove: Option[Int] = value.split(' ').lift(5).flatMap(_.toIntOption)

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

  def owareStoneArray: Array[Int] =
    (
      value.split(' ')(0).split('/')(1).split(',')
        ++
          value.split(' ')(0).split('/')(0).split(',').reverse
    )
      .map(c =>
        c.toString() match {
          case x if 1 to 6 map (_.toString) contains x => Array.fill(x.toInt)(0)
          case _                                       => Array(c.dropRight(1).toInt)
        }
      )
      .flatten
      .toArray

  def initial = value == Forsyth.initial.value
}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)
}
