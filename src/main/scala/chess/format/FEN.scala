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

  // NOTE: the check count has two positions. Field 6, after the move counters, is the current
  // format: checks given, e.g. "+1+0". Field 4, after en passant, is the older format still found
  // in stored five-check FENs: checks remaining, e.g. "4+5".
  def checkCountField: Option[String] =
    List(4, 6).flatMap(value.split(' ').lift).find(_.contains('+'))

  def checksGiven: Option[Score] =
    checkCountField match {
      case None                               => Some(Score())
      case Some(FEN.ChecksGivenField(p1, p2)) => Some(Score(p1.toInt, p2.toInt))
      case Some(_)                            => None
    }

  def withChecksRemaining(checksToWin: Int): FEN =
    checksGiven.fold(this) { checks =>
      val fields    = value.split(' ').toList.filterNot(_.contains('+'))
      val remaining = s"${checksToWin - checks.p1}+${checksToWin - checks.p2}"
      FEN((fields.take(4) ::: remaining :: fields.drop(4)).mkString(" "))
    }
}

object FEN {

  private val ChecksGivenField = """\+(\d)\+(\d)""".r

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)

  // NOTE: Fairy-Stockfish reads a checks-given count ("+1+0") as out of three whatever the variant,
  // so five-check must reach it as checks remaining ("4+5"). Three-check needs no change.
  def fishnetFen(variant: strategygames.chess.variant.Variant)(fen: FEN): FEN = variant match {
    case strategygames.chess.variant.FiveCheck => fen.withChecksRemaining(5)
    case _                                     => fen
  }
}
