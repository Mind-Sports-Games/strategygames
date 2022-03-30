package strategygames.opening

import cats.syntax.option._

import strategygames.GameLogic
import strategygames.format.FEN

object FullOpeningDB {

  def findByFen(lib: GameLogic, fen: FEN): Option[FullOpening] = (lib, fen) match {
    case (GameLogic.Draughts(), _)
      => strategygames.draughts.opening.FullOpeningDB.findByFen(fen.value).map(
        FullOpening.Draughts
      ) 
    case (GameLogic.Chess(), FEN.Chess(fen))
      => strategygames.chess.opening.FullOpeningDB.findByFen(fen).map(
        FullOpening.Chess
      )
    case (GameLogic.FairySF(), FEN.FairySF(fen))
      => strategygames.fairysf.opening.FullOpeningDB.findByFen(fen).map(
        FullOpening.FairySF
      )
    case (GameLogic.Mancala(), FEN.Mancala(fen))
      => strategygames.mancala.opening.FullOpeningDB.findByFen(fen).map(
        FullOpening.Mancala
      )
    case _ => sys.error("Mismatched gamelogic types full opening db")
  }

  // assumes standard initial FEN and variant
  def search(lib: GameLogic, moveStrs: Iterable[String]): Option[FullOpening.AtPly] =
    lib match {
      case GameLogic.Draughts() => strategygames.draughts.opening.FullOpeningDB.search(
        moveStrs
      ).map(fo => FullOpening.AtPly(FullOpening.Draughts(fo.opening), fo.ply))
      case GameLogic.Chess()    => strategygames.chess.opening.FullOpeningDB.search(
        moveStrs
      ).map(fo => FullOpening.AtPly(FullOpening.Chess(fo.opening), fo.ply))
      case GameLogic.FairySF()  => strategygames.fairysf.opening.FullOpeningDB.search(
        moveStrs
      ).map(fo => FullOpening.AtPly(FullOpening.FairySF(fo.opening), fo.ply))
      case GameLogic.Mancala()  => strategygames.mancala.opening.FullOpeningDB.search(
        moveStrs
      ).map(fo => FullOpening.AtPly(FullOpening.Mancala(fo.opening), fo.ply))
    }

  private def draughtsFENs(fens: Vector[FEN]): Vector[strategygames.draughts.format.FEN] =
    fens.flatMap(f =>
      f match {
        case f: FEN.Draughts => Some(f.f)
        case _               => None
      }
    )

  private def chessFENs(fens: Vector[FEN]): Vector[strategygames.chess.format.FEN] =
    fens.flatMap(f =>
      f match {
        case f: FEN.Chess => Some(f.f)
        case _            => None
      }
    )

  private def fairysfFENs(fens: Vector[FEN]): Vector[strategygames.fairysf.format.FEN] =
    fens.flatMap(f =>
      f match {
        case f: FEN.FairySF => Some(f.f)
        case _              => None
      }
    )
  
  private def mancalaFENs(fens: Vector[FEN]): Vector[strategygames.mancala.format.FEN] =
    fens.flatMap(f =>
      f match {
        case f: FEN.Mancala => Some(f.f)
        case _              => None
      }
    )

  def searchInFens(lib: GameLogic, fens: Vector[FEN]): Option[FullOpening] = lib match {
    case GameLogic.Draughts() => strategygames.draughts.opening.FullOpeningDB.searchInFens(
      draughtsFENs(fens).toList
    ).map(FullOpening.Draughts)
    case GameLogic.Chess()    => strategygames.chess.opening.FullOpeningDB.searchInFens(
      chessFENs(fens)
    ).map(FullOpening.Chess)
    case GameLogic.FairySF()  => strategygames.fairysf.opening.FullOpeningDB.searchInFens(
      fairysfFENs(fens)
    ).map(FullOpening.FairySF)
    case GameLogic.Mancala()  => strategygames.mancala.opening.FullOpeningDB.searchInFens(
      mancalaFENs(fens)
    ).map(FullOpening.Mancala)
  }

}
