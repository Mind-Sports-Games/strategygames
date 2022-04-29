package strategygames.format

import strategygames.{ Player, GameLogic }
import strategygames.variant.Variant

abstract sealed class FEN(val value: String) {

  def toChess: strategygames.chess.format.FEN
  def toDraughts: strategygames.draughts.format.FEN
  def toFairySF: strategygames.fairysf.format.FEN
  def toMancala: strategygames.mancala.format.FEN

  override def toString = value

  def fullMove: Option[Int]

  def player: Option[Player]

  def ply: Option[Int]

  def initial: Boolean

  def chessFen: Option[strategygames.chess.format.FEN]

  def player1Score: Int
  def player2Score: Int

}

object FEN {

  final case class Chess(f: strategygames.chess.format.FEN) extends FEN(f.value) {

    def toChess = f
    def toDraughts = sys.error("Can't convert chess to draughts")
    def toFairySF = sys.error("Can't convert chess to fairysf")
    def toMancala = sys.error("Can't convert chess to mancala")

    def fullMove: Option[Int] = f.fullMove

    def player: Option[Player] = f.player

    def ply: Option[Int] = f.ply

    def initial: Boolean = f.initial

    def chessFen: Option[strategygames.chess.format.FEN] = Some(f)

    def player1Score = ???
    def player2Score = ???

  }

  final case class Draughts(f: strategygames.draughts.format.FEN) extends FEN(f.value) {

    def toChess = sys.error("Can't convert draughts to chess")
    def toDraughts = f
    def toFairySF = sys.error("Can't convert draughts to fairysf")
    def toMancala = sys.error("Can't convert draughts to mancala")

    //need to consider an implementation for draughts?
    def fullMove: Option[Int] = None

    def player: Option[Player] = f.player

    //need to consider an implementation for draughts?
    def ply: Option[Int] = None

    def initial: Boolean = f.initial

    def chessFen: Option[strategygames.chess.format.FEN] = None

    def player1Score = ???
    def player2Score = ???

  }

  final case class FairySF(f: strategygames.fairysf.format.FEN) extends FEN(f.value) {

    def toChess = sys.error("Can't convert fairysf to chess")
    def toDraughts = sys.error("Can't convert fairysf to draughts")
    def toFairySF = f
    def toMancala = sys.error("Can't convert fairysf to mancala")

    def fullMove: Option[Int] = f.fullMove

    def player: Option[Player] = f.player

    def ply: Option[Int] = f.ply

    def initial: Boolean = f.initial

    def chessFen: Option[strategygames.chess.format.FEN] = None

    def player1Score = ???
    def player2Score = ???

  }

  final case class Mancala(f: strategygames.mancala.format.FEN) extends FEN(f.value) {

    def toChess = sys.error("Can't convert mancala to chess")
    def toDraughts = sys.error("Can't convert mancala to draughts")
    def toFairySF = sys.error("Can't convert mancala to fairysf")
    def toMancala = f

    def fullMove: Option[Int] = ??? //f.fullmove

    def player: Option[Player] = f.player

    def ply: Option[Int] = ??? //f.ply

    def initial: Boolean = f.initial

    def chessFen: Option[strategygames.chess.format.FEN] = None

    def player1Score = f.player1Score
    def player2Score = f.player2Score

  }

  def wrap(fen: strategygames.chess.format.FEN) = Chess(fen)
  def wrap(fen: strategygames.draughts.format.FEN) = Draughts(fen)
  def wrap(fen: strategygames.fairysf.format.FEN) = FairySF(fen)
  def wrap(fen: strategygames.mancala.format.FEN) = Mancala(fen)

  def apply(lib: GameLogic, value: String): FEN = lib match {
    case GameLogic.Draughts() => FEN.Draughts(strategygames.draughts.format.FEN(value))
    case GameLogic.Chess()    => FEN.Chess(strategygames.chess.format.FEN(value))
    case GameLogic.FairySF()  => FEN.FairySF(strategygames.fairysf.format.FEN(value))
    case GameLogic.Mancala()  => FEN.Mancala(strategygames.mancala.format.FEN(value))
  }

  def apply(v: Variant, value: String): FEN = apply(v.gameLogic, value)

  def clean(lib: GameLogic, source: String): FEN = lib match {
    case GameLogic.Draughts()
      => Draughts(strategygames.draughts.format.FEN(source.replace("_", " ").trim))
    case GameLogic.Chess()
      => Chess(strategygames.chess.format.FEN(source.replace("_", " ").trim))
    case GameLogic.FairySF()
      => FairySF(strategygames.fairysf.format.FEN(source.replace("_", " ").trim))
    case GameLogic.Mancala()
      => Mancala(strategygames.mancala.format.FEN(source.replace("_", " ").trim))
  }

}
