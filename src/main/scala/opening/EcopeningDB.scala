package strategygames.opening
import strategygames.GameLogic

object EcopeningDB {

    import Ecopening._
    val MAX_MOVES = 25

    def allByEco(lib : GameLogic): Map[ECO, Ecopening] = lib match {
        case GameLogic.Chess() => strategygames.chess.opening.EcopeningDB.allByEco.map{ case (a,b) => a -> Ecopening.Chess(b)}
        case GameLogic.Draughts() => strategygames.draughts.opening.EcopeningDB.allByEco.map{ case (a,b) => a -> Ecopening.Draughts(b)}
        case GameLogic.FairySF() => strategygames.fairysf.opening.EcopeningDB.allByEco.map{ case (a,b) => a -> Ecopening.FairySF(b)}
        case GameLogic.Mancala() => strategygames.mancala.opening.EcopeningDB.allByEco.map{ case (a,b) => a -> Ecopening.Mancala(b)}
        case _ => sys.error("Mismatched gamelogic types ecopening db")
    }

    def allByFen(lib : GameLogic): Map[FEN, Ecopening] = lib match {
        case GameLogic.Chess() => strategygames.chess.opening.EcopeningDB.allByFen.map{ case (a,b) => a -> Ecopening.Chess(b)}
        case GameLogic.Draughts() => strategygames.draughts.opening.EcopeningDB.allByFen.map{ case (a,b) => a -> Ecopening.Draughts(b)}
        case GameLogic.FairySF() => strategygames.fairysf.opening.EcopeningDB.allByFen.map{ case (a,b) => a -> Ecopening.FairySF(b)}
        case GameLogic.Mancala() => strategygames.mancala.opening.EcopeningDB.allByFen.map{ case (a,b) => a -> Ecopening.Mancala(b)}
        case _ => sys.error("Mismatched gamelogic types ecopening db")
    }

    def all(lib : GameLogic): List[Ecopening] = lib match {
        case GameLogic.Chess() => strategygames.chess.opening.EcopeningDB.all.map(Ecopening.Chess)
        case GameLogic.Draughts() => strategygames.draughts.opening.EcopeningDB.all.map(Ecopening.Draughts)
        case GameLogic.FairySF() => strategygames.fairysf.opening.EcopeningDB.all.map(Ecopening.FairySF)
        case GameLogic.Mancala() => strategygames.mancala.opening.EcopeningDB.all.map(Ecopening.Mancala)
        case _ => sys.error("Mismatched gamelogic types ecopening db")
    }

}