package strategygames.format

import strategygames._
import strategygames.variant.Variant

import cats.implicits._

sealed trait Uci {

  def uci: String
  def shortUci: String
  def fishnetUci: String
  def piotr: String

  def origDest: (Pos, Pos)

  // TODO: Again, unsafe but we'll get back to it.
  def toChess: chess.format.Uci
  def toDraughts: draughts.format.Uci
  def toFairySF: fairysf.format.Uci
  def toSamurai: samurai.format.Uci
  def toTogyzkumalak: togyzkumalak.format.Uci
  def toGo: go.format.Uci
  def toBackgammon: backgammon.format.Uci

}

object Uci {

  sealed trait Chess        {
    def unwrap: chess.format.Uci
  }
  sealed trait Draughts     {
    def unwrap: draughts.format.Uci
  }
  sealed trait FairySF      {
    def unwrap: fairysf.format.Uci
  }
  sealed trait Samurai      {
    def unwrap: samurai.format.Uci
  }
  sealed trait Togyzkumalak {
    def unwrap: togyzkumalak.format.Uci
  }
  sealed trait Go           {
    def unwrap: go.format.Uci
  }
  sealed trait Backgammon   {
    def unwrap: backgammon.format.Uci
  }

  sealed abstract class Move(
      val orig: Pos,
      val dest: Pos,
      val promotion: Option[PromotableRole] = None,
      val capture: Option[List[Pos]] = None
  ) extends Uci {

    def keys = orig.key + dest.key
    def uci: String
    def fishnetUci: String

    def keysPiotr = orig.piotrStr + dest.piotrStr
    def piotr     = keysPiotr + promotionString

    def promotionString = promotion.fold("")(_.forsyth.toString)

    def origDest = orig -> dest

  }

  final case class ChessMove(m: chess.format.Uci.Move)
      extends Move(
        Pos.Chess(m.orig),
        Pos.Chess(m.dest),
        m.promotion.map(Role.ChessPromotableRole)
      )
      with Chess {
    def uci            = m.uci
    def shortUci       = m.uci
    def fishnetUci     = m.uci
    val unwrap         = m
    def toChess        = m
    def toDraughts     = sys.error("Can't make a draughts UCI from a chess UCI")
    def toFairySF      = sys.error("Can't make a fairysf UCI from a chess UCI")
    def toSamurai      = sys.error("Can't make a samurai UCI from a chess UCI")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak UCI from a chess UCI")
    def toGo           = sys.error("Can't make a go UCI from a chess UCI")
    def toBackgammon   = sys.error("Can't make a backgammon UCI from a chess UCI")
  }

  final case class DraughtsMove(m: draughts.format.Uci.Move)
      extends Move(
        Pos.Draughts(m.orig),
        Pos.Draughts(m.dest),
        m.promotion.map(Role.DraughtsPromotableRole),
        m.capture match {
          case Some(capture) => Some(capture.map(Pos.Draughts))
          case None          => None
        }
      )
      with Draughts {
    def uci            = m.uci
    def shortUci       = m.shortUci
    def fishnetUci     = m.uci
    val unwrap         = m
    def toDraughts     = m
    def toChess        = sys.error("Can't make a chess UCI from a draughts UCI")
    def toFairySF      = sys.error("Can't make a fairysf UCI from a draughts UCI")
    def toSamurai      = sys.error("Can't make a samurai UCI from a draughts UCI")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak UCI from a draughts UCI")
    def toGo           = sys.error("Can't make a go UCI from a draughts UCI")
    def toBackgammon   = sys.error("Can't make a backgammon UCI from a draughts UCI")
  }

  final case class FairySFMove(m: fairysf.format.Uci.Move)
      extends Move(
        Pos.FairySF(m.orig),
        Pos.FairySF(m.dest),
        m.promotion.map(Role.FairySFPromotableRole)
      )
      with FairySF {
    def uci            = m.uci
    def shortUci       = m.uci
    def fishnetUci     = m.fishnetUci
    val unwrap         = m
    def toChess        = sys.error("Can't make a chess UCI from a fairysf UCI")
    def toDraughts     = sys.error("Can't make a draughts UCI from a fairysf UCI")
    def toFairySF      = m
    def toSamurai      = sys.error("Can't make a samurai UCI from a fairy UCI")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak UCI from a fairy UCI")
    def toGo           = sys.error("Can't make a go UCI from a fairy UCI")
    def toBackgammon   = sys.error("Can't make a backgammon UCI from a fairysf UCI")
  }

  final case class SamuraiMove(m: samurai.format.Uci.Move)
      extends Move(
        Pos.Samurai(m.orig),
        Pos.Samurai(m.dest)
      )
      with Samurai {
    def uci            = m.uci
    def shortUci       = m.uci
    def fishnetUci     = m.uci
    val unwrap         = m
    def toChess        = sys.error("Can't make a chess UCI from a samurai UCI")
    def toDraughts     = sys.error("Can't make a draughts UCI from a samurai UCI")
    def toFairySF      = sys.error("Can't make a fairysf UCI from a samurai UCI")
    def toSamurai      = m
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak UCI from a samurai UCI")
    def toGo           = sys.error("Can't make a go UCI from a samurai UCI")
    def toBackgammon   = sys.error("Can't make a backgammon UCI from a samurai UCI")
  }

  final case class TogyzkumalakMove(m: togyzkumalak.format.Uci.Move)
      extends Move(
        Pos.Togyzkumalak(m.orig),
        Pos.Togyzkumalak(m.dest)
      )
      with Togyzkumalak {
    def uci            = m.uci
    def shortUci       = m.uci
    def fishnetUci     = m.uci
    val unwrap         = m
    def toChess        = sys.error("Can't make a chess UCI from a togyzkumalak UCI")
    def toDraughts     = sys.error("Can't make a draughts UCI from a togyzkumalak UCI")
    def toFairySF      = sys.error("Can't make a fairysf UCI from a togyzkumalak UCI")
    def toSamurai      = sys.error("Can't make a samurai UCI from a togyzkumalak UCI")
    def toTogyzkumalak = m
    def toGo           = sys.error("Can't make a go UCI from a togyzkumalak UCI")
    def toBackgammon   = sys.error("Can't make a backgammon UCI from a togyzkumalak UCI")
  }

  final case class BackgammonMove(m: backgammon.format.Uci.Move)
      extends Move(
        Pos.Backgammon(m.orig),
        Pos.Backgammon(m.dest)
      )
      with Backgammon {
    def uci            = m.uci
    def shortUci       = m.uci
    def fishnetUci     = m.uci
    val unwrap         = m
    def toChess        = sys.error("Can't make a chess UCI from a backgammon UCI")
    def toDraughts     = sys.error("Can't make a draughts UCI from a backgammon UCI")
    def toFairySF      = sys.error("Can't make a fairysf UCI from a backgammon UCI")
    def toSamurai      = sys.error("Can't make a samurai UCI from a backgammon UCI")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak UCI from a backgammon UCI")
    def toGo           = sys.error("Can't make a go UCI from a backgammon UCI")
    def toBackgammon   = m
  }

  sealed abstract class Drop(
      val role: Role,
      val pos: Pos
  ) extends Uci {
    def origDest = pos -> pos
  }

  final case class ChessDrop(d: chess.format.Uci.Drop)
      extends Drop(
        Role.ChessRole(d.role),
        Pos.Chess(d.pos)
      )
      with Chess {
    def uci            = d.uci
    def shortUci       = d.uci
    def fishnetUci     = d.uci
    def piotr          = d.piotr
    val unwrap         = d
    def toChess        = d
    def toDraughts     = sys.error("Can't make a draughts UCI from a chess UCI")
    def toFairySF      = sys.error("Can't make a fairysf UCI from a chess UCI")
    def toSamurai      = sys.error("Can't make a samurai UCI from a chess UCI")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak UCI from a chess UCI")
    def toGo           = sys.error("Can't make a go UCI from a chess UCI")
    def toBackgammon   = sys.error("Can't make a backgammon UCI from a chess UCI")
  }

  final case class FairySFDrop(d: fairysf.format.Uci.Drop)
      extends Drop(
        Role.FairySFRole(d.role),
        Pos.FairySF(d.pos)
      )
      with FairySF {
    def uci            = d.uci
    def shortUci       = d.uci
    def fishnetUci     = d.fishnetUci
    def piotr          = d.piotr
    val unwrap         = d
    def toChess        = sys.error("Can't make a chess UCI from a fairysf UCI")
    def toDraughts     = sys.error("Can't make a draughts UCI from a fairysf UCI")
    def toFairySF      = d
    def toSamurai      = sys.error("Can't make a samurai UCI from a fairysf UCI")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak UCI from a fairysf UCI")
    def toGo           = sys.error("Can't make a go UCI from a fairysf UCI")
    def toBackgammon   = sys.error("Can't make a backgammon UCI from a fairysf UCI")
  }

  final case class GoDrop(d: go.format.Uci.Drop)
      extends Drop(
        Role.GoRole(d.role),
        Pos.Go(d.pos)
      )
      with Go {
    def uci            = d.uci
    def shortUci       = d.uci
    def fishnetUci     = d.fishnetUci
    def piotr          = d.piotr
    val unwrap         = d
    def toChess        = sys.error("Can't make a chess UCI from a go UCI")
    def toDraughts     = sys.error("Can't make a draughts UCI from a go UCI")
    def toFairySF      = sys.error("Can't make a fairy UCI from a go UCI")
    def toSamurai      = sys.error("Can't make a samurai UCI from a go UCI")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak UCI from a go UCI")
    def toGo           = d
    def toBackgammon   = sys.error("Can't make a backgammon UCI from a go UCI")
  }

  sealed abstract class Pass() extends Uci {
    def origDest: (Pos, Pos)
  }

  final case class GoPass(p: go.format.Uci.Pass) extends Pass() with Go {
    def uci        = p.uci
    def shortUci   = p.uci
    def fishnetUci = p.uci
    def piotr      = p.piotr

    def origDest: (Pos, Pos) = (Pos.Go(p.origDest._1), Pos.Go(p.origDest._2))
    val unwrap               = p

    def toChess        = sys.error("Can't make a chess UCI from a go UCI")
    def toDraughts     = sys.error("Can't make a draughts UCI from a go UCI")
    def toFairySF      = sys.error("Can't make a fairysf UCI from a go UCI")
    def toSamurai      = sys.error("Can't make a samurai UCI from a go UCI")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak UCI from a go UCI")
    def toGo           = p
    def toBackgammon   = sys.error("Can't make a backgammon UCI from a go UCI")
  }

  sealed abstract class SelectSquares(
      val squares: List[Pos]
  ) extends Uci {
    def origDest: (Pos, Pos)
  }

  final case class GoSelectSquares(ss: go.format.Uci.SelectSquares)
      extends SelectSquares(
        ss.squares.map(Pos.Go(_))
      )
      with Go {
    def uci        = ss.uci
    def shortUci   = ss.uci
    def fishnetUci = ss.uci
    def piotr      = ss.piotr

    def origDest: (Pos, Pos) = (Pos.Go(ss.origDest._1), Pos.Go(ss.origDest._2))
    val unwrap               = ss

    def toChess        = sys.error("Can't make a chess UCI from a go UCI")
    def toDraughts     = sys.error("Can't make a draughts UCI from a go UCI")
    def toFairySF      = sys.error("Can't make a fairysf UCI from a go UCI")
    def toSamurai      = sys.error("Can't make a samurai UCI from a go UCI")
    def toTogyzkumalak = sys.error("Can't make a togyzkumalak UCI from a go UCI")
    def toGo           = ss
    def toBackgammon   = sys.error("Can't make a backgammon UCI from a go UCI")
  }

  def wrap(uci: chess.format.Uci): Uci = uci match {
    case m: chess.format.Uci.Move => ChessMove(m)
    case d: chess.format.Uci.Drop => ChessDrop(d)
  }

  def wrap(uci: draughts.format.Uci): Uci = uci match {
    case m: draughts.format.Uci.Move => DraughtsMove(m)
  }

  def wrap(uci: fairysf.format.Uci): Uci = uci match {
    case m: fairysf.format.Uci.Move => FairySFMove(m)
    case d: fairysf.format.Uci.Drop => FairySFDrop(d)
  }

  def wrap(uci: samurai.format.Uci): Uci = uci match {
    case m: samurai.format.Uci.Move => SamuraiMove(m)
  }

  def wrap(uci: togyzkumalak.format.Uci): Uci = uci match {
    case m: togyzkumalak.format.Uci.Move => TogyzkumalakMove(m)
  }

  def wrap(uci: go.format.Uci): Uci = uci match {
    case d: go.format.Uci.Drop           => GoDrop(d)
    case p: go.format.Uci.Pass           => GoPass(p)
    case ss: go.format.Uci.SelectSquares => GoSelectSquares(ss)
  }

  def wrap(uci: backgammon.format.Uci): Uci = uci match {
    case m: backgammon.format.Uci.Move => BackgammonMove(m)
  }

  object Move {

    private def draughtsCaptures(captures: Option[List[Pos]]): Option[List[draughts.Pos]] =
      captures match {
        case Some(captures) =>
          Some(
            captures.flatMap(c =>
              c match {
                case Pos.Draughts(c) => Some(c)
                case _               => None
              }
            )
          )
        case None           => None
      }

    def apply(
        lib: GameLogic,
        orig: Pos,
        dest: Pos,
        promotion: Option[PromotableRole],
        capture: Option[List[Pos]] = None
    ): Move =
      (lib, orig, dest) match {
        case (GameLogic.Draughts(), Pos.Draughts(orig), Pos.Draughts(dest))             =>
          DraughtsMove(
            draughts.format.Uci.Move(
              orig,
              dest,
              promotion.map(_.toDraughts),
              draughtsCaptures(capture)
            )
          )
        case (GameLogic.Chess(), Pos.Chess(orig), Pos.Chess(dest))                      =>
          ChessMove(
            chess.format.Uci.Move(
              orig,
              dest,
              promotion.map(_.toChess)
            )
          )
        case (GameLogic.FairySF(), Pos.FairySF(orig), Pos.FairySF(dest))                =>
          FairySFMove(
            fairysf.format.Uci.Move(
              orig,
              dest,
              promotion.map(_.toFairySF)
            )
          )
        case (GameLogic.Samurai(), Pos.Samurai(orig), Pos.Samurai(dest))                =>
          SamuraiMove(
            samurai.format.Uci.Move.apply(
              orig,
              dest,
              promotion.map(_.toSamurai)
            )
          )
        case (GameLogic.Togyzkumalak(), Pos.Togyzkumalak(orig), Pos.Togyzkumalak(dest)) =>
          TogyzkumalakMove(
            togyzkumalak.format.Uci.Move.apply(
              orig,
              dest,
              promotion.map(_.toTogyzkumalak)
            )
          )
        case (GameLogic.Backgammon(), Pos.Backgammon(orig), Pos.Backgammon(dest))       =>
          BackgammonMove(
            backgammon.format.Uci.Move.apply(
              orig,
              dest,
              promotion.map(_.toBackgammon)
            )
          )
        case _                                                                          => sys.error("Mismatched gamelogic types 23")
      }

    def apply(lib: GameLogic, gf: GameFamily, move: String): Option[Move] = lib match {
      case GameLogic.Draughts()     => draughts.format.Uci.Move(move).map(DraughtsMove)
      case GameLogic.Chess()        => chess.format.Uci.Move(move).map(ChessMove)
      case GameLogic.FairySF()      => fairysf.format.Uci.Move(gf, move).map(FairySFMove)
      case GameLogic.Samurai()      => samurai.format.Uci.Move(move).map(SamuraiMove)
      case GameLogic.Togyzkumalak() => togyzkumalak.format.Uci.Move(move).map(TogyzkumalakMove)
      case GameLogic.Backgammon()   => backgammon.format.Uci.Move(move).map(BackgammonMove)
    }

    def piotr(lib: GameLogic, gf: GameFamily, move: String): Option[Move] = lib match {
      case GameLogic.Draughts()     => draughts.format.Uci.Move.piotr(move).map(DraughtsMove)
      case GameLogic.Chess()        => chess.format.Uci.Move.piotr(move).map(ChessMove)
      case GameLogic.FairySF()      => fairysf.format.Uci.Move.piotr(gf, move).map(FairySFMove)
      case GameLogic.Samurai()      => samurai.format.Uci.Move.piotr(move).map(SamuraiMove)
      case GameLogic.Togyzkumalak() => togyzkumalak.format.Uci.Move.piotr(move).map(TogyzkumalakMove)
      case GameLogic.Backgammon()   => backgammon.format.Uci.Move.piotr(move).map(BackgammonMove)
    }

    def fromStrings(
        lib: GameLogic,
        gf: GameFamily,
        origS: String,
        destS: String,
        promS: Option[String]
    ): Option[Move] = lib match {
      case GameLogic.Draughts()     => draughts.format.Uci.Move.fromStrings(origS, destS, promS).map(DraughtsMove)
      case GameLogic.Chess()        => chess.format.Uci.Move.fromStrings(origS, destS, promS).map(ChessMove)
      case GameLogic.FairySF()      =>
        fairysf.format.Uci.Move.fromStrings(gf, origS, destS, promS).map(FairySFMove)
      case GameLogic.Samurai()      =>
        samurai.format.Uci.Move.fromStrings(gf, origS, destS, promS).map(SamuraiMove)
      case GameLogic.Togyzkumalak() =>
        togyzkumalak.format.Uci.Move.fromStrings(gf, origS, destS, promS).map(TogyzkumalakMove)
      case GameLogic.Go()           => None
      case GameLogic.Backgammon()   =>
        backgammon.format.Uci.Move.fromStrings(gf, origS, destS, promS).map(BackgammonMove)
    }
  }

  object Drop {

    def fromStrings(lib: GameLogic, gf: GameFamily, roleS: String, posS: String): Option[Drop] =
      lib match {
        case GameLogic.Draughts()     => None
        case GameLogic.Samurai()      => None
        case GameLogic.Togyzkumalak() => None
        case GameLogic.Backgammon()   => None
        case GameLogic.Chess()        =>
          chess.format.Uci.Drop.fromStrings(roleS, posS).map(ChessDrop)
        case GameLogic.FairySF()      =>
          fairysf.format.Uci.Drop.fromStrings(gf, roleS, posS).map(FairySFDrop)
        case GameLogic.Go()           =>
          go.format.Uci.Drop.fromStrings(roleS, posS).map(GoDrop)
      }

  }

  object Pass {

    def apply(lib: GameLogic, gf: GameFamily): Option[Pass] =
      lib match {
        case GameLogic.Draughts()     => None
        case GameLogic.Samurai()      => None
        case GameLogic.Togyzkumalak() => None
        case GameLogic.Backgammon()   => None
        case GameLogic.Chess()        => None
        case GameLogic.FairySF()      => None
        case GameLogic.Go()           => GoPass(go.format.Uci.Pass()).some
      }

  }

  object SelectSquares {

    def fromSquares(lib: GameLogic, gf: GameFamily, squares: List[Pos]): Option[SelectSquares] =
      lib match {
        case GameLogic.Draughts()     => None
        case GameLogic.Samurai()      => None
        case GameLogic.Togyzkumalak() => None
        case GameLogic.Backgammon()   => None
        case GameLogic.Chess()        => None
        case GameLogic.FairySF()      => None
        case GameLogic.Go()           =>
          GoSelectSquares(
            go.format.Uci.SelectSquares
              .fromSquares(
                squares.map(p =>
                  p match {
                    case Pos.Go(pos) => pos
                    case _           => sys.error("Not passed Go pos objects")
                  }
                )
              )
          ).some

      }

  }

  sealed abstract class WithSan(val uci: Uci, val san: String)

  final case class ChessWithSan(w: chess.format.Uci.WithSan)
      extends WithSan(
        wrap(w.uci),
        w.san
      )

  final case class DraughtsWithSan(w: draughts.format.Uci.WithSan)
      extends WithSan(
        wrap(w.uci),
        w.san
      )

  final case class FairySFWithSan(w: fairysf.format.Uci.WithSan)
      extends WithSan(
        wrap(w.uci),
        w.san
      )

  final case class SamuraiWithSan(w: samurai.format.Uci.WithSan)
      extends WithSan(
        wrap(w.uci),
        w.san
      )

  final case class TogyzkumalakWithSan(w: togyzkumalak.format.Uci.WithSan)
      extends WithSan(
        wrap(w.uci),
        w.san
      )

  final case class GoWithSan(w: go.format.Uci.WithSan)
      extends WithSan(
        wrap(w.uci),
        w.san
      )

  final case class BackgammonWithSan(w: backgammon.format.Uci.WithSan)
      extends WithSan(
        wrap(w.uci),
        w.san
      )

  object WithSan {

    def apply(lib: GameLogic, uci: Uci, san: String): WithSan = (lib, uci) match {
      case (GameLogic.Draughts(), Uci.DraughtsMove(uci))   =>
        Uci.DraughtsWithSan(draughts.format.Uci.WithSan(uci, san))
      case (GameLogic.Chess(), u: Uci.Chess)               => Uci.ChessWithSan(chess.format.Uci.WithSan(u.unwrap, san))
      case (GameLogic.FairySF(), u: Uci.FairySF)           =>
        Uci.FairySFWithSan(fairysf.format.Uci.WithSan(u.unwrap, san))
      case (GameLogic.Samurai(), u: Uci.Samurai)           =>
        Uci.SamuraiWithSan(samurai.format.Uci.WithSan(u.unwrap, san))
      case (GameLogic.Togyzkumalak(), u: Uci.Togyzkumalak) =>
        Uci.TogyzkumalakWithSan(togyzkumalak.format.Uci.WithSan(u.unwrap, san))
      case (GameLogic.Go(), u: Uci.Go)                     =>
        Uci.GoWithSan(go.format.Uci.WithSan(u.unwrap, san))
      case (GameLogic.Backgammon(), u: Uci.Backgammon)     =>
        Uci.BackgammonWithSan(backgammon.format.Uci.WithSan(u.unwrap, san))
      case _                                               => sys.error("Mismatched gamelogic types 24")
    }

  }

  def apply(lib: GameLogic, move: strategygames.Move, withCaptures: Boolean = false): Uci.Move =
    (lib, move) match {
      case (GameLogic.Draughts(), strategygames.Move.Draughts(move))         =>
        DraughtsMove(draughts.format.Uci(move, withCaptures))
      case (GameLogic.Chess(), strategygames.Move.Chess(move))               => ChessMove(chess.format.Uci(move))
      case (GameLogic.FairySF(), strategygames.Move.FairySF(move))           =>
        FairySFMove(fairysf.format.Uci(move))
      case (GameLogic.Samurai(), strategygames.Move.Samurai(move))           =>
        SamuraiMove(samurai.format.Uci(move))
      case (GameLogic.Togyzkumalak(), strategygames.Move.Togyzkumalak(move)) =>
        TogyzkumalakMove(togyzkumalak.format.Uci(move))
      case (GameLogic.Go(), _)                                               => sys.error("Move not implemented for Go")
      case (GameLogic.Backgammon(), strategygames.Move.Backgammon(move))     =>
        BackgammonMove(backgammon.format.Uci(move))
      case _                                                                 => sys.error("Mismatched gamelogic types 25")
    }

  def apply(lib: GameLogic, drop: strategygames.Drop) = (lib, drop) match {
    case (GameLogic.Draughts(), _)                               => sys.error("Drop not implemented for Draughts")
    case (GameLogic.Chess(), strategygames.Drop.Chess(drop))     => ChessDrop(chess.format.Uci(drop))
    case (GameLogic.FairySF(), strategygames.Drop.FairySF(drop)) =>
      FairySFDrop(fairysf.format.Uci(drop))
    case (GameLogic.Samurai(), _)                                => sys.error("Drop not implemented for samurai")
    case (GameLogic.Togyzkumalak(), _)                           => sys.error("Drop not implemented for togyzkumalak")
    case (GameLogic.Go(), strategygames.Drop.Go(drop))           => GoDrop(go.format.Uci(drop))
    case (GameLogic.Backgammon(), _)                             => sys.error("Drop not implemented for backgammon")
  }

  def apply(lib: GameLogic, pass: strategygames.Pass) = (lib, pass) match {
    case (GameLogic.Draughts(), _)                     => sys.error("Pass not implemented for Draughts")
    case (GameLogic.Chess(), _)                        => sys.error("Pass not implemented for Chess")
    case (GameLogic.FairySF(), _)                      => sys.error("Pass not implemented for fairysf")
    case (GameLogic.Samurai(), _)                      => sys.error("Pass not implemented for samurai")
    case (GameLogic.Togyzkumalak(), _)                 => sys.error("Pass not implemented for togyzkumalak")
    case (GameLogic.Go(), strategygames.Pass.Go(pass)) => GoPass(go.format.Uci(pass))
    case (GameLogic.Backgammon(), _)                   => sys.error("Pass not implemented for backgammon")
  }

  def apply(lib: GameLogic, selectSquares: strategygames.SelectSquares) = (lib, selectSquares) match {
    case (GameLogic.Draughts(), _)                                       => sys.error("SelectSquares not implemented for Draughts")
    case (GameLogic.Chess(), _)                                          => sys.error("SelectSquares not implemented for Chess")
    case (GameLogic.FairySF(), _)                                        => sys.error("SelectSquares not implemented for fairysf")
    case (GameLogic.Samurai(), _)                                        => sys.error("SelectSquares not implemented for samurai")
    case (GameLogic.Togyzkumalak(), _)                                   => sys.error("SelectSquares not implemented for togyzkumalak")
    case (GameLogic.Go(), strategygames.SelectSquares.Go(selectSquares)) =>
      GoSelectSquares(go.format.Uci(selectSquares))
    case (GameLogic.Backgammon(), _)                                     => sys.error("SelectSquares not implemented for backgammon")
  }

  def apply(lib: GameLogic, gf: GameFamily, move: String): Option[Uci] = lib match {
    case GameLogic.Draughts()     => draughts.format.Uci(move).map(wrap)
    case GameLogic.Chess()        => chess.format.Uci(move).map(wrap)
    case GameLogic.FairySF()      => fairysf.format.Uci(gf, move).map(wrap)
    case GameLogic.Samurai()      => samurai.format.Uci(move).map(wrap)
    case GameLogic.Togyzkumalak() => togyzkumalak.format.Uci(move).map(wrap)
    case GameLogic.Go()           => go.format.Uci(move).map(wrap)
    case GameLogic.Backgammon()   => backgammon.format.Uci(move).map(wrap)
  }

  def apply(v: Variant, move: String): Option[Uci] =
    apply(v.gameLogic, v.gameFamily, move)

  private def piotr(lib: GameLogic, gf: GameFamily, move: String): Option[Uci] = lib match {
    case GameLogic.Draughts()     => draughts.format.Uci.piotr(move).map(wrap)
    case GameLogic.Chess()        => chess.format.Uci.piotr(move).map(wrap)
    case GameLogic.FairySF()      => fairysf.format.Uci.piotr(gf, move).map(wrap)
    case GameLogic.Samurai()      => samurai.format.Uci.piotr(move).map(wrap)
    case GameLogic.Togyzkumalak() => togyzkumalak.format.Uci.piotr(move).map(wrap)
    case GameLogic.Go()           => go.format.Uci.piotr(move).map(wrap)
    case GameLogic.Backgammon()   => backgammon.format.Uci.piotr(move).map(wrap)
  }

  def readList(lib: GameLogic, gf: GameFamily, moves: String): Option[List[Uci]] = lib match {
    case GameLogic.Draughts()     => draughts.format.Uci.readList(moves).map(_.map(wrap))
    case GameLogic.Chess()        => chess.format.Uci.readList(moves).map(_.map(wrap))
    case GameLogic.FairySF()      => fairysf.format.Uci.readList(gf, moves).map(_.map(wrap))
    case GameLogic.Samurai()      => samurai.format.Uci.readList(moves).map(_.map(wrap))
    case GameLogic.Togyzkumalak() => togyzkumalak.format.Uci.readList(moves).map(_.map(wrap))
    case GameLogic.Go()           => go.format.Uci.readList(moves).map(_.map(wrap))
    case GameLogic.Backgammon()   => backgammon.format.Uci.readList(moves).map(_.map(wrap))
  }

  def writeList(moves: List[Uci]): String =
    moves.map(_.uci) mkString " "

  def readListPiotr(moves: String): Option[List[Uci]] =
    moves.split('_') match {
      case Array(lib, gf, moves) =>
        moves
          .split(' ')
          .toList
          .map(
            piotr(GameLogic(lib.toInt), GameFamily(gf.toInt), _)
          )
          .sequence
      case _                     => sys.error("No lib encoded into uci piotr")
    }

  def writeListPiotr(gf: GameFamily, moves: List[Uci]): String =
    (if (moves.length > 0) {
       moves.head match {
         case Uci.ChessMove(_)        => s"0_${gf.id}_"
         case Uci.ChessDrop(_)        => s"0_${gf.id}_"
         case Uci.DraughtsMove(_)     => s"1_${gf.id}_"
         case Uci.FairySFMove(_)      => s"2_${gf.id}_"
         case Uci.FairySFDrop(_)      => s"2_${gf.id}_"
         case Uci.SamuraiMove(_)      => s"3_${gf.id}_"
         case Uci.TogyzkumalakMove(_) => s"4_${gf.id}_"
         case Uci.GoDrop(_)           => s"5_${gf.id}_"
         case Uci.GoPass(_)           => s"5_${gf.id}_"
         case Uci.GoSelectSquares(_)  => s"5_${gf.id}_"
         case Uci.BackgammonMove(_)   => s"6_${gf.id}_"
       }
     } else "") + (moves.map(_.piotr) mkString " ")

}
