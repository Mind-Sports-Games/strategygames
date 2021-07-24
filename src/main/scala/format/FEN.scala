package strategygames.format

import strategygames.{ Color, GameLib }

abstract sealed class FEN(val value: String) {

  override def toString = value

  def fullMove: Option[Int]

  def color: Option[Color]

  def ply: Option[Int]

  def initial: Boolean

  def chessFen: Option[strategygames.chess.format.FEN]

}

object FEN {

  final case class Chess(f: strategygames.chess.format.FEN) extends FEN(f.value) {

    def fullMove: Option[Int] = f.fullMove

    def color: Option[Color] = f.color

    def ply: Option[Int] = f.ply

    def initial: Boolean = f.initial

    def chessFen: Option[strategygames.chess.format.FEN] = Some(f)

  }

  final case class Draughts(f: strategygames.draughts.format.FEN) extends FEN(f.value) {

    //need to consider an implementation for draughts?
    def fullMove: Option[Int] = None

    def color: Option[Color] = f.color

    //need to consider an implementation for draughts?
    def ply: Option[Int] = None

    def initial: Boolean = f.initial

    def chessFen: Option[strategygames.chess.format.FEN] = None

  }

  def wrap(fen: strategygames.chess.format.FEN) = Chess(fen)
  def wrap(fen: strategygames.draughts.format.FEN) = Draughts(fen)

  def apply(lib: GameLib, value: String): FEN = lib match {
    case GameLib.Draughts() => FEN.Draughts(strategygames.draughts.format.FEN(value))
    case GameLib.Chess()    => FEN.Chess(strategygames.chess.format.FEN(value))
  }

  def clean(lib: GameLib, source: String): FEN = lib match {
    case GameLib.Draughts()
      => Draughts(strategygames.draughts.format.FEN(source.replace("_", " ").trim))
    case GameLib.Chess()
      => Chess(strategygames.chess.format.FEN(source.replace("_", " ").trim))
  }

}
