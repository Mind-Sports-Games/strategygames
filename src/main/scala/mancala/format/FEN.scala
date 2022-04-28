package strategygames.mancala.format

import strategygames.Player

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  //def fullMove: Option[Int] = value.split(' ').lift(5).flatMap(_.toIntOption)

  def player: Option[Player] =
    value.split(' ').lift(3) flatMap (_.headOption) flatMap Player.apply

  def player1Score: Int = 
    value.split(' ').lift(1) flatMap (_.headOption) match {
      case Some(v) => 
        v.toString() match {
          case "1"  => 0
          case c if c.matches("[A-Z]") => v.toInt - 64
          case c if c.matches("[a-z]") => v.toInt - 70
          case _ => 0
          }
      case None => 0
      }

  def player2Score: Int = 
    value.split(' ').lift(2) flatMap (_.headOption) match {
      case Some(value) => 
        value.toString() match {
          case "1"  => 0
          case c if c.matches("[A-Z]") => value.toInt - 64
          case c if c.matches("[a-z]") => value.toInt - 70
          case _ => 0
          }
      case None => 0
      }
  
  def owareStoneArray: Array[Int] = 
    (
      value.split(' ')(0).split('/')(1)
      +
      value.split(' ')(0).split('/')(0).reverse
    )  
    .map(c =>
      c.toString() match{
        case "1" => Array.fill(1)(0) //empty
        case "2" => Array.fill(2)(0)
        case "3" => Array.fill(3)(0)
        case "4" => Array.fill(4)(0)
        case "5" => Array.fill(5)(0)
        case "6" => Array.fill(6)(0)
        case _ =>
          c.toInt match{
            case y if y <= 90 => Array(y - 64)
            case y if y > 96  => Array(y - 70)
          } 
      }
    ).flatten.toArray

  
  // def ply: Option[Int] =
  //   fullMove map { fm =>
  //     fm * 2 - (if (player.exists(_.p1)) 2 else 1)
  //   }

  def initial = value == Forsyth.initial.value
}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)
}
