package strategygames.mancala.format

import strategygames.Player

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  //def fullMove: Option[Int] = value.split(' ').lift(5).flatMap(_.toIntOption)

  def player: Option[Player] =
    value.split(' ').lift(3) flatMap (_.headOption) flatMap Player.apply

  def player1Score: Int = 
    value.split(' ').lift(1) flatMap (_.headOption) match {
      case Some(value) => 
        value.toString() match {
          case "1"  => 0
          case c if c.matches("A-Z") => value.toInt - 64
          case c if c.matches("a-z") => value.toInt - 70
          case _ => 0
          }
      case None => 0
      }

  def player2Score: Int = 
    value.split(' ').lift(2) flatMap (_.headOption) match {
      case Some(value) => 
        value.toString() match {
          case "1"  => 0
          case c if c.matches("A-Z") => value.toInt - 64
          case c if c.matches("a-z") => value.toInt - 70
          case _ => 0
          }
      case None => 0
      }

  // def ply: Option[Int] =
  //   fullMove map { fm =>
  //     fm * 2 - (if (player.exists(_.p1)) 2 else 1)
  //   }

  def initial = value == Forsyth.initial.value
}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)
}
