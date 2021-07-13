package strategygames.format

import strategygames.Color

sealed class FEN(val value: String) extends AnyVal {

  override def toString = value

  def color: Option[Color]

  def initial = value == Forsyth.initial.value

}

object FEN {

  final case class Chess(f: chess.FEN) extends FEN(f.value) {
    def color: Option[Color] = f.color.map(Color.Chess)
  }

  final case class Draughts(f: draughts.FEN) extends FEN(f.value) {
    def color: Option[Color] = f.color.map(Color.Draughts)
  }

  def clean(source: String): FEN = FEN(source.replace("_", " ").trim)

}
