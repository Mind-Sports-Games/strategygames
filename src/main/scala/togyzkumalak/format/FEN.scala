package strategygames.togyzkumalak.format

import strategygames.Player

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  // def fullMove: Option[Int] = value.split(' ').lift(5).flatMap(_.toIntOption)

  def player: Option[Player] =
    value.split(' ').lift(3) flatMap (_.headOption) flatMap Player.apply

  def player1Score: Int = playerScore(1)

  def player2Score: Int = playerScore(2)

  private def playerScore(playerIndex: Int): Int =
    value
      .split(' ')
      .lift(playerIndex)
      .map(_.toInt)
      .getOrElse(0)

  private def width: Int = 9

  def mancalaStoneArray: Array[Int] =
    (
      value.split(' ')(0).split('/')(1).split(',')
        ++
          value.split(' ')(0).split('/')(0).split(',').reverse
    )
      .map(c =>
        c.toString() match {
          case x if 1 to width map (_.toString) contains x => Array.fill(x.toInt)(0)
          case x if x.length > 1                           => Array(c.dropRight(1).toInt)
          case _                                           => Array(0)
        }
      )
      .flatten
      .toArray

  private def tuzdikPit(playerFen: Array[String]): Option[Int] = {
    val pit = playerFen
      .map(c =>
        c.toString() match {
          case x if 1 to width map (_.toString) contains x => Array.fill(x.toInt)(0)
          case x if x.length > 1                           => Array(0)
          case _                                           => Array(1)
        }
      )
      .flatten
      .toArray
      .indexOf(1)
    if (pit < 0) None else Some(pit)
  }

  def tuzdikPits: Map[Player, Option[Int]] =
    Map(
      Player.P1 -> tuzdikPit(value.split(' ')(0).split('/')(1).split(',')),
      Player.P2 -> tuzdikPit(value.split(' ')(0).split('/')(0).split(',').reverse)
    )

  // def ply: Option[Int] =
  //   fullMove map { fm =>
  //     fm * 2 - (if (player.exists(_.p1)) 2 else 1)
  //   }

  def initial = value == Forsyth.initial.value
}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)
}
