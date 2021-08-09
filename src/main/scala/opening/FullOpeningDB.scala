package strategygames.opening

import cats.syntax.option._

import strategygames.GameLib
import strategygames.format.FEN

object FullOpeningDB {

  def findByFen(lib: GameLib, fen: FEN): Option[FullOpening] = (lib, fen) match {
    case (GameLib.Draughts(), _)
      => strategygames.draughts.opening.FullOpeningDB.findByFen(fen.value).map(
        FullOpening.Draughts
      ) 
    case (GameLib.Chess(), FEN.Chess(fen))
      => strategygames.chess.opening.FullOpeningDB.findByFen(fen).map(
        FullOpening.Chess
      )
    case _ => sys.error("Mismatched gamelib types full opening db")
  }

  // assumes standard initial FEN and variant
  def search(lib: GameLib, moveStrs: Iterable[String]): Option[FullOpening.AtPly] =
    lib match {
      case GameLib.Draughts() => strategygames.draughts.opening.FullOpeningDB.search(
        moveStrs
      ).map(fo => FullOpening.AtPly(FullOpening.Draughts(fo.opening), fo.ply))
      case GameLib.Chess()    => strategygames.chess.opening.FullOpeningDB.search(
        moveStrs
      ).map(fo => FullOpening.AtPly(FullOpening.Chess(fo.opening), fo.ply))
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

  def searchInFens(lib: GameLib, fens: Vector[FEN]): Option[FullOpening] = lib match {
    case GameLib.Draughts() => strategygames.draughts.opening.FullOpeningDB.searchInFens(
      draughtsFENs(fens).toList
    ).map(FullOpening.Draughts)
    case GameLib.Chess()    => strategygames.chess.opening.FullOpeningDB.searchInFens(
      chessFENs(fens)
    ).map(FullOpening.Chess)
  }

}
