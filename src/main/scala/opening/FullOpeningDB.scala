package strategygames.opening

import strategygames.{ ActionStrs, GameLogic }
import strategygames.format.FEN

object FullOpeningDB {

  def findByFen(lib: GameLogic, fen: FEN): Option[FullOpening] = (lib, fen) match {
    case (GameLogic.Draughts(), _)                         =>
      strategygames.draughts.opening.FullOpeningDB
        .findByFen(fen.value)
        .map(
          FullOpening.Draughts
        )
    case (GameLogic.Chess(), FEN.Chess(fen))               =>
      strategygames.chess.opening.FullOpeningDB
        .findByFen(fen)
        .map(
          FullOpening.Chess
        )
    case (GameLogic.FairySF(), FEN.FairySF(fen))           =>
      strategygames.fairysf.opening.FullOpeningDB
        .findByFen(fen)
        .map(
          FullOpening.FairySF
        )
    case (GameLogic.Samurai(), FEN.Samurai(fen))           =>
      strategygames.samurai.opening.FullOpeningDB
        .findByFen(fen)
        .map(
          FullOpening.Samurai
        )
    case (GameLogic.Togyzkumalak(), FEN.Togyzkumalak(fen)) =>
      strategygames.togyzkumalak.opening.FullOpeningDB
        .findByFen(fen)
        .map(
          FullOpening.Togyzkumalak
        )
    case (GameLogic.Go(), FEN.Go(fen))                     =>
      strategygames.go.opening.FullOpeningDB
        .findByFen(fen)
        .map(
          FullOpening.Go
        )
    case (GameLogic.Backgammon(), FEN.Backgammon(fen))     =>
      strategygames.backgammon.opening.FullOpeningDB
        .findByFen(fen)
        .map(
          FullOpening.Backgammon
        )
    case (GameLogic.Abalone(), FEN.Abalone(fen))           =>
      strategygames.abalone.opening.FullOpeningDB
        .findByFen(fen)
        .map(
          FullOpening.Abalone
        )
    case (GameLogic.Dameo(), FEN.Dameo(fen))           =>
      strategygames.dameo.opening.FullOpeningDB
        .findByFen(fen)
        .map(
          FullOpening.Dameo
        )
    case _                                                 => sys.error("Mismatched gamelogic types full opening db")
  }

  // assumes standard initial FEN and variant
  def search(lib: GameLogic, actionStrs: ActionStrs): Option[FullOpening.AtPly] =
    lib match {
      case GameLogic.Draughts()     =>
        strategygames.draughts.opening.FullOpeningDB
          .search(actionStrs)
          .map(fo => FullOpening.AtPly(FullOpening.Draughts(fo.opening), fo.ply))
      case GameLogic.Chess()        =>
        strategygames.chess.opening.FullOpeningDB
          .search(actionStrs)
          .map(fo => FullOpening.AtPly(FullOpening.Chess(fo.opening), fo.ply))
      case GameLogic.FairySF()      =>
        strategygames.fairysf.opening.FullOpeningDB
          .search(actionStrs)
          .map(fo => FullOpening.AtPly(FullOpening.FairySF(fo.opening), fo.ply))
      case GameLogic.Samurai()      =>
        strategygames.samurai.opening.FullOpeningDB
          .search(actionStrs)
          .map(fo => FullOpening.AtPly(FullOpening.Samurai(fo.opening), fo.ply))
      case GameLogic.Togyzkumalak() =>
        strategygames.togyzkumalak.opening.FullOpeningDB
          .search(actionStrs)
          .map(fo => FullOpening.AtPly(FullOpening.Togyzkumalak(fo.opening), fo.ply))
      case GameLogic.Go()           =>
        strategygames.go.opening.FullOpeningDB
          .search(actionStrs)
          .map(fo => FullOpening.AtPly(FullOpening.Go(fo.opening), fo.ply))
      case GameLogic.Backgammon()   =>
        strategygames.backgammon.opening.FullOpeningDB
          .search(actionStrs)
          .map(fo => FullOpening.AtPly(FullOpening.Backgammon(fo.opening), fo.ply))
      case GameLogic.Abalone()      =>
        strategygames.abalone.opening.FullOpeningDB
          .search(actionStrs)
          .map(fo => FullOpening.AtPly(FullOpening.Abalone(fo.opening), fo.ply))
      case GameLogic.Dameo()        =>
        strategygames.dameo.opening.FullOpeningDB
          .search(actionStrs)
          .map(fo => FullOpening.AtPly(FullOpening.Dameo(fo.opening), fo.ply))
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

  private def samuraiFENs(fens: Vector[FEN]): Vector[strategygames.samurai.format.FEN] =
    fens.flatMap(f =>
      f match {
        case f: FEN.Samurai => Some(f.f)
        case _              => None
      }
    )

  private def togyzkumalakFENs(fens: Vector[FEN]): Vector[strategygames.togyzkumalak.format.FEN] =
    fens.flatMap(f =>
      f match {
        case f: FEN.Togyzkumalak => Some(f.f)
        case _                   => None
      }
    )

  private def goFENs(fens: Vector[FEN]): Vector[strategygames.go.format.FEN] =
    fens.flatMap(f =>
      f match {
        case f: FEN.Go => Some(f.f)
        case _         => None
      }
    )

  private def backgammonFENs(fens: Vector[FEN]): Vector[strategygames.backgammon.format.FEN] =
    fens.flatMap(f =>
      f match {
        case f: FEN.Backgammon => Some(f.f)
        case _                 => None
      }
    )

  private def abaloneFENs(fens: Vector[FEN]): Vector[strategygames.abalone.format.FEN] =
    fens.flatMap(f =>
      f match {
        case f: FEN.Abalone => Some(f.f)
        case _              => None
      }
    )

  private def dameoFENs(fens: Vector[FEN]): Vector[strategygames.dameo.format.FEN] =
    fens.flatMap(f =>
      f match {
        case f: FEN.Dameo => Some(f.f)
        case _            => None
      }
    )

  def searchInFens(lib: GameLogic, fens: Vector[FEN]): Option[FullOpening] = lib match {
    case GameLogic.Draughts()     =>
      strategygames.draughts.opening.FullOpeningDB
        .searchInFens(
          draughtsFENs(fens).toList
        )
        .map(FullOpening.Draughts)
    case GameLogic.Chess()        =>
      strategygames.chess.opening.FullOpeningDB
        .searchInFens(
          chessFENs(fens)
        )
        .map(FullOpening.Chess)
    case GameLogic.FairySF()      =>
      strategygames.fairysf.opening.FullOpeningDB
        .searchInFens(
          fairysfFENs(fens)
        )
        .map(FullOpening.FairySF)
    case GameLogic.Samurai()      =>
      strategygames.samurai.opening.FullOpeningDB
        .searchInFens(
          samuraiFENs(fens)
        )
        .map(FullOpening.Samurai)
    case GameLogic.Togyzkumalak() =>
      strategygames.togyzkumalak.opening.FullOpeningDB
        .searchInFens(
          togyzkumalakFENs(fens)
        )
        .map(FullOpening.Togyzkumalak)
    case GameLogic.Go()           =>
      strategygames.go.opening.FullOpeningDB
        .searchInFens(
          goFENs(fens)
        )
        .map(FullOpening.Go)
    case GameLogic.Backgammon()   =>
      strategygames.backgammon.opening.FullOpeningDB
        .searchInFens(
          backgammonFENs(fens)
        )
        .map(FullOpening.Backgammon)
    case GameLogic.Abalone()      =>
      strategygames.abalone.opening.FullOpeningDB
        .searchInFens(
          abaloneFENs(fens)
        )
        .map(FullOpening.Abalone)
    case GameLogic.Dameo()        =>
      strategygames.dameo.opening.FullOpeningDB
        .searchInFens(
          dameoFENs(fens)
        )
        .map(FullOpening.Dameo)
  }

}
