package strategygames.mancala.format

import strategygames.Player

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  // def fullMove: Option[Int] = value.split(' ').lift(5).flatMap(_.toIntOption)

  def player: Option[Player] =
    value.split(' ').lift(3) flatMap (_.headOption) flatMap Player.apply

  def player1Score: Int = playerScore("p1")

  def player2Score: Int = playerScore("p2")

  private def playerScore(playerIndex: String): Int =
    value
      .split(' ')
      .lift(if (playerIndex == "p1") 1 else 2)
      .flatMap(_.headOption)
      .fold(0)(v =>
        v.toString() match {
          case "1"                     => 0
          case c if c.matches("[A-Z]") => v.toInt - 64
          case c if c.matches("[a-z]") => v.toInt - 70
          case _                       => 0
        }
      )

  def owareStoneArray: Array[Int] =
    (
      value.split(' ')(0).split('/')(1)
        +
          value.split(' ')(0).split('/')(0).reverse
    )
      .map(c =>
        c.toString() match {
          case x if 1 to 6 map (_.toString) contains x => Array.fill(x.toInt)(0)
          case _                                       =>
            c.toInt match {
              case y if y <= 90 => Array(y - 64)
              case y if y > 96  => Array(y - 70)
            }
        }
      )
      .flatten
      .toArray

  def gameEndPlayer1Score: Int = {
    player1Score + owareStoneArray.take(6).sum
  }

  def gameEndPlayer2Score: Int = {
    player2Score + owareStoneArray.drop(6).take(6).sum
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
