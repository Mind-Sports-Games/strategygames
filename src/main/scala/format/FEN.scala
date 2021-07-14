package strategygames.format

import strategygames.{ Color, GameLib }

abstract sealed class FEN(val value: String) {

  override def toString = value

  def color: Option[Color]

  def initial: Boolean

}

object FEN {

  final case class Chess(f: strategygames.chess.format.FEN) extends FEN(f.value) {

    def color: Option[Color] = f.color.map(Color.Chess)

    def initial: Boolean = f.initial

  }

  final case class Draughts(f: strategygames.draughts.format.FEN) extends FEN(f.value) {

    def color: Option[Color] = f.color.map(Color.Draughts)

    def initial: Boolean = f.initial

  }

  def clean(lib: GameLib, source: String): FEN = lib match {
    case GameLib.Draughts()
      => Draughts(strategygames.draughts.format.FEN(source.replace("_", " ").trim))
    case GameLib.Chess()
      => Chess(strategygames.chess.format.FEN(source.replace("_", " ").trim))
  }

}
