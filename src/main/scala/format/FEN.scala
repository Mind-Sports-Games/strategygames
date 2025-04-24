package strategygames.format

import strategygames.variant.Variant
import strategygames.{GameLogic, Player}

sealed abstract class FEN(val value: String) {

  def toChess: strategygames.chess.format.FEN
  def toDraughts: strategygames.draughts.format.FEN
  def toFairySF: strategygames.fairysf.format.FEN
  def toSamurai: strategygames.samurai.format.FEN
  def toTogyzkumalak: strategygames.togyzkumalak.format.FEN
  def toGo: strategygames.go.format.FEN
  def toBackgammon: strategygames.backgammon.format.FEN
  def toAbalone: strategygames.abalone.format.FEN
  def toDameo: strategygames.dameo.format.FEN

  override def toString = value

  def gameLogic: GameLogic

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

    def toChess        = f
    def toDraughts     = sys.error("Can't convert chess to draughts")
    def toFairySF      = sys.error("Can't convert chess to fairysf")
    def toSamurai      = sys.error("Can't convert chess to samurai")
    def toTogyzkumalak = sys.error("Can't convert chess to togyzkumalak")
    def toGo           = sys.error("Can't convert chess to go")
    def toBackgammon   = sys.error("Can't convert chess to backgammon")
    def toAbalone      = sys.error("Can't convert chess to abalone")
    def toDameo        = sys.error("Can't convert chess to dameo")

    def gameLogic = GameLogic.Chess()

    def fullMove: Option[Int] = f.fullMove

    def player: Option[Player] = f.player

    def ply: Option[Int] = f.ply

    def initial: Boolean = f.initial

    def chessFen: Option[strategygames.chess.format.FEN] = Some(f)

    def player1Score = sys.error("There is no player1 score in chess")
    def player2Score = sys.error("There is no player2 score in chess")

  }

  final case class Draughts(f: strategygames.draughts.format.FEN) extends FEN(f.value) {

    def toChess        = sys.error("Can't convert draughts to chess")
    def toDraughts     = f
    def toFairySF      = sys.error("Can't convert draughts to fairysf")
    def toSamurai      = sys.error("Can't convert draughts to samurai")
    def toTogyzkumalak = sys.error("Can't convert draughts to togyzkumalak")
    def toGo           = sys.error("Can't convert draughts to go")
    def toBackgammon   = sys.error("Can't convert draughts to backgammon")
    def toAbalone      = sys.error("Can't convert draughts to abalone")
    def toDameo        = sys.error("Can't convert draughts to dameo")

    def gameLogic = GameLogic.Draughts()

    // need to consider an implementation for draughts?
    def fullMove: Option[Int] = None

    def player: Option[Player] = f.player

    // need to consider an implementation for draughts?
    def ply: Option[Int] = None

    def initial: Boolean = f.initial

    def chessFen: Option[strategygames.chess.format.FEN] = None

    def player1Score = sys.error("There is no player1 score in draughts")
    def player2Score = sys.error("There is no player2 score in draughts")

  }

  final case class FairySF(f: strategygames.fairysf.format.FEN) extends FEN(f.value) {

    def toChess        = sys.error("Can't convert fairysf to chess")
    def toDraughts     = sys.error("Can't convert fairysf to draughts")
    def toFairySF      = f
    def toSamurai      = sys.error("Can't convert fairysf to samurai")
    def toTogyzkumalak = sys.error("Can't convert fairysf to togyzkumalak")
    def toGo           = sys.error("Can't convert fairysf to go")
    def toBackgammon   = sys.error("Can't convert fairysf to backgammon")
    def toAbalone      = sys.error("Can't convert fairysf to abalone")
    def toDameo        = sys.error("Can't convert fairysf to dameo")

    def gameLogic = GameLogic.FairySF()

    def fullMove: Option[Int] = f.fullMove

    def player: Option[Player] = f.player

    def ply: Option[Int] = f.ply

    def initial: Boolean = f.initial

    def chessFen: Option[strategygames.chess.format.FEN] = None

    def player1Score = sys.error("There is no player1 score in fairy")
    def player2Score = sys.error("There is no player2 score in fairy")

  }

  final case class Samurai(f: strategygames.samurai.format.FEN) extends FEN(f.value) {

    def toChess        = sys.error("Can't convert samurai to chess")
    def toDraughts     = sys.error("Can't convert samurai to draughts")
    def toFairySF      = sys.error("Can't convert samurai to fairysf")
    def toSamurai      = f
    def toTogyzkumalak = sys.error("Can't convert samurai to togyzkumalak")
    def toGo           = sys.error("Can't convert samurai to go")
    def toBackgammon   = sys.error("Can't convert samurai to backgammon")
    def toAbalone      = sys.error("Can't convert samurai to abalone")
    def toDameo        = sys.error("Can't convert samurai to dameo")

    def gameLogic = GameLogic.Samurai()

    def fullMove: Option[Int] = sys.error("There is no fullMove in samurai")

    def player: Option[Player] = f.player

    def ply: Option[Int] = sys.error("There is no ply in samurai")

    def initial: Boolean = f.initial

    def chessFen: Option[strategygames.chess.format.FEN] = None

    def player1Score = f.player1Score
    def player2Score = f.player2Score

  }

  final case class Togyzkumalak(f: strategygames.togyzkumalak.format.FEN) extends FEN(f.value) {

    def toChess        = sys.error("Can't convert togyzkumalak to chess")
    def toDraughts     = sys.error("Can't convert togyzkumalak to draughts")
    def toFairySF      = sys.error("Can't convert togyzkumalak to fairysf")
    def toSamurai      = sys.error("Can't convert togyzkumalak to samurai")
    def toTogyzkumalak = f
    def toGo           = sys.error("Can't convert togyzkumalak to go")
    def toBackgammon   = sys.error("Can't convert togyzkumalak to backgammon")
    def toAbalone      = sys.error("Can't convert togyzkumalak to abalone")
    def toDameo        = sys.error("Can't convert togyzkumalak to dameo")

    def gameLogic = GameLogic.Togyzkumalak()

    def fullMove: Option[Int] = sys.error("There is no fullMove in togyzkumalak")

    def player: Option[Player] = f.player

    def ply: Option[Int] = sys.error("There is no ply in togy")

    def initial: Boolean = f.initial

    def chessFen: Option[strategygames.chess.format.FEN] = None

    def player1Score = f.player1Score
    def player2Score = f.player2Score

  }

  final case class Go(f: strategygames.go.format.FEN) extends FEN(f.value) {

    def toChess        = sys.error("Can't convert go to chess")
    def toDraughts     = sys.error("Can't convert go to draughts")
    def toFairySF      = sys.error("Can't convert go to fairysf")
    def toSamurai      = sys.error("Can't convert go to samurai")
    def toTogyzkumalak = sys.error("Can't convert go to togyzkumalak")
    def toGo           = f
    def toBackgammon   = sys.error("Can't convert go to backgammon")
    def toAbalone      = sys.error("Can't convert go to abalone")
    def toDameo        = sys.error("Can't convert go to dameo")

    def gameLogic = GameLogic.Go()

    def fullMove: Option[Int] = sys.error("There is no fullMove in go")

    def player: Option[Player] = f.player

    def ply: Option[Int] = sys.error("There is no ply in go")

    def initial: Boolean = f.initial

    def chessFen: Option[strategygames.chess.format.FEN] = None

    def player1Score = f.player1Score
    def player2Score = f.player2Score

  }

  final case class Backgammon(f: strategygames.backgammon.format.FEN) extends FEN(f.value) {

    def toChess        = sys.error("Can't convert backgammon to chess")
    def toDraughts     = sys.error("Can't convert backgammon to draughts")
    def toFairySF      = sys.error("Can't convert backgammon to fairysf")
    def toSamurai      = sys.error("Can't convert backgammon to samurai")
    def toTogyzkumalak = sys.error("Can't convert backgammon to togyzkumalak")
    def toGo           = sys.error("Can't convert backgammon to go")
    def toBackgammon   = f
    def toAbalone      = sys.error("Can't convert backgammon to abalone")
    def toDameo        = sys.error("Can't convert backgammon to dameo")

    def gameLogic = GameLogic.Backgammon()

    def fullMove: Option[Int] = sys.error("There is no fullMove in backgammon")

    def player: Option[Player] = f.player

    def ply: Option[Int] = sys.error("There is no ply in backgammon")

    def initial: Boolean = f.initial

    def chessFen: Option[strategygames.chess.format.FEN] = None

    def player1Score = f.player1Score
    def player2Score = f.player2Score

  }

  final case class Abalone(f: strategygames.abalone.format.FEN) extends FEN(f.value) {
    override def toChess        = sys.error("Can't convert abalone to chess")
    override def toDraughts     = sys.error("Can't convert abalone to draughts")
    override def toFairySF      = sys.error("Can't convert abalone to fairysf")
    override def toSamurai      = sys.error("Can't convert abalone to samurai")
    override def toTogyzkumalak = sys.error("Can't convert abalone to togyzkumalak")
    override def toGo           = sys.error("Can't convert abalone to go")
    override def toBackgammon   = sys.error("Can't convert abalone to backgammon")
    override def toAbalone      = f
    override def toDameo        = sys.error("Can't convert abalone to dameo")

    override def gameLogic = GameLogic.Abalone()

    override def fullMove: Option[Int] = sys.error("There is no fullMove in abalone")

    override def player: Option[Player] = f.player

    override def ply: Option[Int] = sys.error("There is no ply in abalone")

    override def initial: Boolean = f.initial

    override def chessFen: Option[strategygames.chess.format.FEN] = None

    override def player1Score = f.player1Score
    override def player2Score = f.player2Score
  }

  final case class Dameo(f: strategygames.dameo.format.FEN) extends FEN(f.value) {

    def toChess        = sys.error("Can't convert dameo to chess")
    def toDraughts     = sys.error("Can't convert dameo to draughts")
    def toFairySF      = sys.error("Can't convert dameo to fairysf")
    def toSamurai      = sys.error("Can't convert dameo to samurai")
    def toTogyzkumalak = sys.error("Can't convert dameo to togyzkumalak")
    def toGo           = sys.error("Can't convert dameo to go")
    def toBackgammon   = sys.error("Can't convert dameo to backgammon")
    def toAbalone      = sys.error("Can't convert dameo to abalone")
    def toDameo        = f

    def gameLogic = GameLogic.Dameo()

    def fullMove: Option[Int] = sys.error("There is no fullMove in dameo")

    def player: Option[Player] = f.player

    def ply: Option[Int] = sys.error("There is no ply in dameo")

    def initial: Boolean = f.initial

    def chessFen: Option[strategygames.chess.format.FEN] = None

    def player1Score = sys.error("There is no player1 score in dameo")
    def player2Score = sys.error("There is no player2 score in dameo")

  }

  def wrap(fen: strategygames.chess.format.FEN)        = Chess(fen)
  def wrap(fen: strategygames.draughts.format.FEN)     = Draughts(fen)
  def wrap(fen: strategygames.fairysf.format.FEN)      = FairySF(fen)
  def wrap(fen: strategygames.samurai.format.FEN)      = Samurai(fen)
  def wrap(fen: strategygames.togyzkumalak.format.FEN) = Togyzkumalak(fen)
  def wrap(fen: strategygames.go.format.FEN)           = Go(fen)
  def wrap(fen: strategygames.backgammon.format.FEN)   = Backgammon(fen)
  def wrap(fen: strategygames.abalone.format.FEN)      = Abalone(fen)
  def wrap(fen: strategygames.dameo.format.FEN)        = Dameo(fen)

  def apply(lib: GameLogic, value: String): FEN = lib match {
    case GameLogic.Draughts()     => FEN.Draughts(strategygames.draughts.format.FEN(value))
    case GameLogic.Chess()        => FEN.Chess(strategygames.chess.format.FEN(value))
    case GameLogic.FairySF()      => FEN.FairySF(strategygames.fairysf.format.FEN(value))
    case GameLogic.Samurai()      => FEN.Samurai(strategygames.samurai.format.FEN(value))
    case GameLogic.Go()           => FEN.Go(strategygames.go.format.FEN(value))
    case GameLogic.Togyzkumalak() => FEN.Togyzkumalak(strategygames.togyzkumalak.format.FEN(value))
    case GameLogic.Backgammon()   => FEN.Backgammon(strategygames.backgammon.format.FEN(value))
    case GameLogic.Abalone()      => FEN.Abalone(strategygames.abalone.format.FEN(value))
    case GameLogic.Dameo()        => FEN.Dameo(strategygames.dameo.format.FEN(value))
  }

  def apply(v: Variant, value: String): FEN = apply(v.gameLogic, value)

  def clean(lib: GameLogic, source: String): FEN = lib match {
    case GameLogic.Draughts()     => Draughts(strategygames.draughts.format.FEN(source.replace("_", " ").trim))
    case GameLogic.Chess()        => Chess(strategygames.chess.format.FEN(source.replace("_", " ").trim))
    case GameLogic.FairySF()      => FairySF(strategygames.fairysf.format.FEN(source.replace("_", " ").trim))
    case GameLogic.Samurai()      => Samurai(strategygames.samurai.format.FEN(source.replace("_", " ").trim))
    case GameLogic.Togyzkumalak() =>
      Togyzkumalak(strategygames.togyzkumalak.format.FEN(source.replace("_", " ").trim))
    case GameLogic.Go()           => Go(strategygames.go.format.FEN(source.replace("_", " ").trim))
    case GameLogic.Backgammon()   =>
      Backgammon(strategygames.backgammon.format.FEN(source.replace("_", " ").trim))
    case GameLogic.Abalone()      =>
      Abalone(strategygames.abalone.format.FEN(source.replace("_", " ").trim))
    case GameLogic.Dameo()        =>
      Dameo(strategygames.dameo.format.FEN(source.replace("_", " ").trim))
  }

  def fishnetFen(variant: Variant)(fen: FEN): FEN = variant match {
    case Variant.FairySF(variant) =>
      wrap(strategygames.fairysf.format.FEN.fishnetFen(variant)(fen.toFairySF))
    case _                        =>
      fen
  }

}
