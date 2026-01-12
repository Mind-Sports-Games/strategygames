package strategygames.opening
import strategygames.GameLogic

object EcopeningDB {

  import Ecopening._
  val MAX_TURNS = 25

  def allByEco(lib: GameLogic): Map[ECO, Ecopening] = lib match {
    case GameLogic.Chess()        =>
      strategygames.chess.opening.EcopeningDB.allByEco.map { case (a, b) => a -> Ecopening.Chess(b) }
    case GameLogic.Draughts()     =>
      strategygames.draughts.opening.EcopeningDB.allByEco.map { case (a, b) => a -> Ecopening.Draughts(b) }
    case GameLogic.FairySF()      =>
      strategygames.fairysf.opening.EcopeningDB.allByEco.map { case (a, b) => a -> Ecopening.FairySF(b) }
    case GameLogic.Samurai()      =>
      strategygames.samurai.opening.EcopeningDB.allByEco.map { case (a, b) => a -> Ecopening.Samurai(b) }
    case GameLogic.Togyzkumalak() =>
      strategygames.togyzkumalak.opening.EcopeningDB.allByEco.map { case (a, b) =>
        a -> Ecopening.Togyzkumalak(b)
      }
    case GameLogic.Go()           =>
      strategygames.go.opening.EcopeningDB.allByEco.map { case (a, b) => a -> Ecopening.Go(b) }
    case GameLogic.Backgammon()   =>
      strategygames.backgammon.opening.EcopeningDB.allByEco.map { case (a, b) =>
        a -> Ecopening.Backgammon(b)
      }
    case GameLogic.Abalone()      =>
      strategygames.abalone.opening.EcopeningDB.allByEco.map { case (a, b) => a -> Ecopening.Abalone(b) }
    case GameLogic.Dameo()        =>
      strategygames.dameo.opening.EcopeningDB.allByEco.map { case (a, b) => a -> Ecopening.Dameo(b) }
    case null                     => sys.error("Mismatched gamelogic types ecopening db")
  }

  def allByFen(lib: GameLogic): Map[FEN, Ecopening] = lib match {
    case GameLogic.Chess()        =>
      strategygames.chess.opening.EcopeningDB.allByFen.map { case (a, b) => a -> Ecopening.Chess(b) }
    case GameLogic.Draughts()     =>
      strategygames.draughts.opening.EcopeningDB.allByFen.map { case (a, b) => a -> Ecopening.Draughts(b) }
    case GameLogic.FairySF()      =>
      strategygames.fairysf.opening.EcopeningDB.allByFen.map { case (a, b) => a -> Ecopening.FairySF(b) }
    case GameLogic.Samurai()      =>
      strategygames.samurai.opening.EcopeningDB.allByFen.map { case (a, b) => a -> Ecopening.Samurai(b) }
    case GameLogic.Togyzkumalak() =>
      strategygames.togyzkumalak.opening.EcopeningDB.allByFen.map { case (a, b) =>
        a -> Ecopening.Togyzkumalak(b)
      }
    case GameLogic.Go()           =>
      strategygames.go.opening.EcopeningDB.allByFen.map { case (a, b) => a -> Ecopening.Go(b) }
    case GameLogic.Backgammon()   =>
      strategygames.backgammon.opening.EcopeningDB.allByFen.map { case (a, b) =>
        a -> Ecopening.Backgammon(b)
      }
    case GameLogic.Abalone()      =>
      strategygames.abalone.opening.EcopeningDB.allByFen.map { case (a, b) => a -> Ecopening.Abalone(b) }
    case GameLogic.Dameo()        =>
      strategygames.dameo.opening.EcopeningDB.allByFen.map { case (a, b) => a -> Ecopening.Dameo(b) }
    case null                     => sys.error("Mismatched gamelogic types ecopening db")
  }

  def all(lib: GameLogic): List[Ecopening] = lib match {
    case GameLogic.Chess()        => strategygames.chess.opening.EcopeningDB.all.map(Ecopening.Chess.apply)
    case GameLogic.Draughts()     => strategygames.draughts.opening.EcopeningDB.all.map(Ecopening.Draughts.apply)
    case GameLogic.FairySF()      => strategygames.fairysf.opening.EcopeningDB.all.map(Ecopening.FairySF.apply)
    case GameLogic.Samurai()      => strategygames.samurai.opening.EcopeningDB.all.map(Ecopening.Samurai.apply)
    case GameLogic.Togyzkumalak() =>
      strategygames.togyzkumalak.opening.EcopeningDB.all.map(Ecopening.Togyzkumalak.apply)
    case GameLogic.Go()           => strategygames.go.opening.EcopeningDB.all.map(Ecopening.Go.apply)
    case GameLogic.Backgammon()   =>
      strategygames.backgammon.opening.EcopeningDB.all.map(Ecopening.Backgammon.apply)
    case GameLogic.Abalone()      => strategygames.abalone.opening.EcopeningDB.all.map(Ecopening.Abalone.apply)
    case GameLogic.Dameo()        => strategygames.dameo.opening.EcopeningDB.all.map(Ecopening.Dameo.apply)
    case null                     => sys.error("Mismatched gamelogic types ecopening db")
  }

}
