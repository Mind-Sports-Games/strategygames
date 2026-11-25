package strategygames.chess.format

import strategygames.{ Player, Score }

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

  def checkCountField: Option[String] =
    List(4, 6).flatMap(value.split(' ').lift).find(_.contains('+'))

  def checksGiven: Option[Score] =
    checkCountField match {
      case None                               => Some(Score())
      case Some(FEN.ChecksGivenField(p1, p2)) => Some(Score(p1.toInt, p2.toInt))
      case Some(_)                            => None
    }
}

object FEN {

  private val ChecksGivenField = """\+(\d)\+(\d)""".r

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)
}
