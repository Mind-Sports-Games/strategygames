package strategygames.draughts.format

import strategygames.draughts.Color

final case class FEN(value: String) extends AnyVal {

  override def toString = value

  def color: Option[Color] =
    value.split(':').lift(0) flatMap (_.headOption) flatMap Color.apply

  def initial = value == Forsyth.initial.value

}

object FEN {

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)

}
