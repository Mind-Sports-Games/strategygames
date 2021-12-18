package strategygames.draughts.format

import strategygames.Player

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  def player: Option[Player] =
    value.split(':').lift(0) flatMap (_.headOption) flatMap Player.apply

  def initial = value == Forsyth.initial.value

}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)

}
