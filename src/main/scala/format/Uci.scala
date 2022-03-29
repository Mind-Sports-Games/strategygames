package strategygames.format

import strategygames._

import cats.implicits._

sealed trait Uci {

  def uci: String
  def piotr: String

  def origDest: (Pos, Pos)

  // TODO: Again, unsafe but we'll get back to it.
  def toChess: chess.format.Uci
  def toDraughts: draughts.format.Uci
  def toFairySF: fairysf.format.Uci

}


object Uci {

  sealed trait Chess {
    def unwrap: chess.format.Uci
  }
  sealed trait Draughts {
    def unwrap: draughts.format.Uci
  }
  sealed trait FairySF {
    def unwrap: fairysf.format.Uci
  }
  sealed trait Oware {
    def unwrap: oware.format.Uci
  }

  abstract sealed class Move(
    val orig: Pos,
    val dest: Pos,
    val promotion: Option[PromotableRole] = None,
    val capture: Option[List[Pos]] = None
  ) extends Uci {

    def keys = orig.key + dest.key
    def uci: String

    def keysPiotr = orig.piotrStr + dest.piotrStr
    def piotr     = keysPiotr + promotionString

    def promotionString = promotion.fold("")(_.forsyth.toString)

    def origDest = orig -> dest

  }

  final case class ChessMove(m: chess.format.Uci.Move) extends Move(
    Pos.Chess(m.orig),
    Pos.Chess(m.dest),
    m.promotion.map(Role.ChessPromotableRole)
  ) with Chess {
    def uci = m.uci
    val unwrap = m
    def toChess = m
    def toDraughts = sys.error("Can't make a draughts UCI from a chess UCI")
    def toFairySF = sys.error("Can't make a fairysf UCI from a chess UCI")
    def toOware = sys.error("Can't make a oware UCI from a chess UCI")
  }

  final case class DraughtsMove(m: draughts.format.Uci.Move) extends Move(
    Pos.Draughts(m.orig),
    Pos.Draughts(m.dest),
    m.promotion.map(Role.DraughtsPromotableRole),
    m.capture match {
      case Some(capture) => Some(capture.map(Pos.Draughts))
      case None          => None
    }
  ) with Draughts {
    def uci = m.uci
    val unwrap = m
    def toDraughts = m
    def toChess = sys.error("Can't make a chess UCI from a draughts UCI")
    def toFairySF = sys.error("Can't make a fairysf UCI from a draughts UCI")
    def toOware = sys.error("Can't make a oware UCI from a draughts UCI")
  }

  final case class FairySFMove(m: fairysf.format.Uci.Move) extends Move(
    Pos.FairySF(m.orig),
    Pos.FairySF(m.dest),
    m.promotion.map(Role.FairySFPromotableRole)
  ) with FairySF {
    def uci = m.uci
    val unwrap = m
    def toChess = sys.error("Can't make a chess UCI from a fairysf UCI")
    def toDraughts = sys.error("Can't make a draughts UCI from a fairysf UCI")
    def toFairySF = m
    def toOware = sys.error("Can't make a oware UCI from a fairy UCI")
  }

  final case class OwareMove(m: oware.format.Uci.Move) extends Move(
    Pos.Oware(m.orig),
    Pos.Oware(m.dest),
    m.promotion.map(Role.OwarePromotableRole)
  ) with Oware {
    def uci = m.uci
    val unwrap = m
    def toChess = sys.error("Can't make a chess UCI from a oware UCI")
    def toDraughts = sys.error("Can't make a draughts UCI from a oware UCI")
    def toFairySF = sys.error("Can't make a fairysf UCI from a oware UCI")
    def toOware = m
  }

  abstract sealed class Drop(
    val role: Role,
    val pos: Pos
  ) extends Uci {
    def origDest = pos -> pos
  }

  final case class ChessDrop(d: chess.format.Uci.Drop) extends Drop(
    Role.ChessRole(d.role),
    Pos.Chess(d.pos)
  ) with Chess {
    def uci                  = d.uci
    def piotr                = d.piotr
    val unwrap = d
    def toChess = d
    def toDraughts = sys.error("Can't make a draughts UCI from a chess UCI")
    def toFairySF = sys.error("Can't make a fairysf UCI from a chess UCI")
    def toOware = sys.error("Can't make a oware UCI from a chess UCI")
  }

  final case class FairySFDrop(d: fairysf.format.Uci.Drop) extends Drop(
    Role.FairySFRole(d.role),
    Pos.FairySF(d.pos)
  ) with FairySF {
    def uci                  = d.uci
    def piotr                = d.piotr
    val unwrap = d
    def toChess = sys.error("Can't make a chess UCI from a fairysf UCI")
    def toDraughts = sys.error("Can't make a draughts UCI from a fairysf UCI")
    def toFairySF = d
    def toOware = sys.error("Can't make a oware UCI from a fairysf UCI")
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

  def wrap(uci: oware.format.Uci): Uci = uci match {
    case m: oware.format.Uci.Move => OwareMove(m)
  }

  object Move {

    private def draughtsCaptures(captures: Option[List[Pos]]): Option[List[draughts.Pos]] =
      captures match {
        case Some(captures) => Some(captures.flatMap(c =>
          c match {
            case Pos.Draughts(c) => Some(c)
            case _               => None
          }
        ))
        case None => None
      }

    def apply(lib: GameLogic, orig: Pos, dest: Pos, promotion: Option[PromotableRole], capture: Option[List[Pos]] = None): Move =
      (lib, orig, dest) match {
        case (GameLogic.Draughts(), Pos.Draughts(orig), Pos.Draughts(dest)) =>
          DraughtsMove(draughts.format.Uci.Move.apply(
            orig, dest, promotion.map(_.toDraughts), draughtsCaptures(capture)
          ))
        case (GameLogic.Chess(), Pos.Chess(orig), Pos.Chess(dest)) =>
          ChessMove(
            chess.format.Uci.Move.apply(
              orig, dest, promotion.map(_.toChess)
            ))
        case (GameLogic.FairySF(), Pos.FairySF(orig), Pos.FairySF(dest)) =>
          FairySFMove(
            fairysf.format.Uci.Move.apply(
              orig, dest, promotion.map(_.toFairySF)
            ))
        case (GameLogic.Oware(), Pos.Oware(orig), Pos.Oware(dest)) =>
          OwareMove(
            oware.format.Uci.Move.apply(
              orig, dest, promotion.map(_.toOware)
            ))
        case _ => sys.error("Mismatched gamelogic types 23")
      }

    def apply(lib: GameLogic, gf: GameFamily, move: String): Option[Move] = lib match {
      case GameLogic.Draughts() => draughts.format.Uci.Move.apply(move).map(DraughtsMove)
      case GameLogic.Chess()    => chess.format.Uci.Move.apply(move).map(ChessMove)
      case GameLogic.FairySF()  => fairysf.format.Uci.Move.apply(gf, move).map(FairySFMove)
      case GameLogic.Oware()    => oware.format.Uci.Move.apply(move).map(OwareMove)
    }

    def piotr(lib: GameLogic, gf: GameFamily, move: String): Option[Move] = lib match {
      case GameLogic.Draughts() => draughts.format.Uci.Move.piotr(move).map(DraughtsMove)
      case GameLogic.Chess()    => chess.format.Uci.Move.piotr(move).map(ChessMove)
      case GameLogic.FairySF()  => fairysf.format.Uci.Move.piotr(gf, move).map(FairySFMove)
      case GameLogic.Oware()    => oware.format.Uci.Move.piotr(move).map(OwareMove)
    }

    def fromStrings(lib: GameLogic, gf: GameFamily, origS: String, destS: String, promS: Option[String]): Option[Move] = lib match {
      case GameLogic.Draughts()
        => draughts.format.Uci.Move.fromStrings(origS, destS, promS).map(DraughtsMove)
      case GameLogic.Chess()
        => chess.format.Uci.Move.fromStrings(origS, destS, promS).map(ChessMove)
      case GameLogic.FairySF()
        => fairysf.format.Uci.Move.fromStrings(gf, origS, destS, promS).map(FairySFMove)
      case GameLogic.Oware()
        => oware.format.Uci.Move.fromStrings(gf, origS, destS, promS).map(OwareMove)
    }
  }

  object Drop {

    def fromStrings(lib: GameLogic, gf: GameFamily, roleS: String, posS: String): Option[Drop] =
      lib match {
        case GameLogic.Draughts() => None
        case GameLogic.Oware() => None
        case GameLogic.Chess()    =>
          chess.format.Uci.Drop.fromStrings(roleS, posS).map(ChessDrop)
        case GameLogic.FairySF()  =>
          fairysf.format.Uci.Drop.fromStrings(gf, roleS, posS).map(FairySFDrop)
      }

  }

  abstract sealed class WithSan(val uci: Uci, val san: String)

  final case class ChessWithSan(w: chess.format.Uci.WithSan) extends WithSan(
    wrap(w.uci),
    w.san
  )

  final case class DraughtsWithSan(w: draughts.format.Uci.WithSan) extends WithSan(
    wrap(w.uci),
    w.san
  )

  final case class FairySFWithSan(w: fairysf.format.Uci.WithSan) extends WithSan(
    wrap(w.uci),
    w.san
  )

  final case class OwareWithSan(w: oware.format.Uci.WithSan) extends WithSan(
    wrap(w.uci),
    w.san
  )

  object WithSan {

    def apply(lib: GameLogic, uci: Uci, san: String): WithSan = (lib, uci) match {
      case (GameLogic.Draughts(), Uci.DraughtsMove(uci))
        => Uci.DraughtsWithSan(draughts.format.Uci.WithSan(uci, san))
      case (GameLogic.Chess(), u: Uci.Chess)
        => Uci.ChessWithSan(chess.format.Uci.WithSan(u.unwrap, san))
      case (GameLogic.FairySF(), u: Uci.FairySF)
        => Uci.FairySFWithSan(fairysf.format.Uci.WithSan(u.unwrap, san))
      case (GameLogic.Oware(), u: Uci.Oware)
        => Uci.OwareWithSan(oware.format.Uci.WithSan(u.unwrap, san))
      case _ => sys.error("Mismatched gamelogic types 24")
    }

  }

  def apply(lib: GameLogic, move: strategygames.Move, withCaptures: Boolean = false): Uci.Move =
    (lib, move) match {
      case (GameLogic.Draughts(), strategygames.Move.Draughts(move))
        => DraughtsMove(draughts.format.Uci.apply(move, withCaptures))
      case (GameLogic.Chess(), strategygames.Move.Chess(move))
        => ChessMove(chess.format.Uci.apply(move))
      case (GameLogic.FairySF(), strategygames.Move.FairySF(move))
        => FairySFMove(fairysf.format.Uci.apply(move))
      case (GameLogic.Oware(), strategygames.Move.Oware(move))
        => OwareMove(oware.format.Uci(move))
      case _ => sys.error("Mismatched gamelogic types 25")
    }

  def apply(lib: GameLogic, drop: strategygames.Drop) = (lib, drop) match {
    case (GameLogic.Draughts(), _) => sys.error("Drop not implemented for Draughts")
    case (GameLogic.Chess(), strategygames.Drop.Chess(drop))
      => ChessDrop(chess.format.Uci.apply(drop))
    case (GameLogic.FairySF(), strategygames.Drop.FairySF(drop))
      => FairySFDrop(fairysf.format.Uci.apply(drop))
    case (GameLogic.Oware(), _) => sys.error("Drop not implemented for Oware")
  }

  def apply(lib: GameLogic, gf: GameFamily, move: String): Option[Uci] = lib match {
      case GameLogic.Draughts() => draughts.format.Uci.apply(move).map(wrap)
      case GameLogic.Chess()    => chess.format.Uci.apply(move).map(wrap)
      case GameLogic.FairySF()  => fairysf.format.Uci.apply(gf, move).map(wrap)
      case GameLogic.Oware()    => oware.format.Uci.apply(move).map(wrap)
  }

  private def piotr(lib: GameLogic, gf: GameFamily, move: String): Option[Uci] = lib match {
      case GameLogic.Draughts() => draughts.format.Uci.piotr(move).map(wrap)
      case GameLogic.Chess()    => chess.format.Uci.piotr(move).map(wrap)
      case GameLogic.FairySF()  => fairysf.format.Uci.piotr(gf, move).map(wrap)
      case GameLogic.Oware()    => oware.format.Uci.piotr(move).map(wrap)
  }

  def readList(lib: GameLogic, gf: GameFamily, moves: String): Option[List[Uci]] =
    moves.split(' ').toList.map(apply(lib, gf, _)).sequence

  def writeList(moves: List[Uci]): String =
    moves.map(_.uci) mkString " "

  def readListPiotr(moves: String): Option[List[Uci]] =
    moves.split('_') match {
      case Array(lib, gf, moves) =>
        moves.split(' ').toList.map(
          piotr(GameLogic(lib.toInt), GameFamily(gf.toInt), _)
        ).sequence
      case _ => sys.error("No lib encoded into uci piotr")
    }

  def writeListPiotr(gf: GameFamily, moves: List[Uci]): String =
    (if (moves.length > 0) {
      moves.head match {
        case Uci.ChessMove(_)    => s"0_${gf.id}_"
        case Uci.ChessDrop(_)    => s"0_${gf.id}_"
        case Uci.DraughtsMove(_) => s"1_${gf.id}_"
        case Uci.FairySFMove(_)  => s"2_${gf.id}_"
        case Uci.FairySFDrop(_)  => s"2_${gf.id}_"
        case Uci.OwareMove(_)    => s"3_${gf.id}_"
      }
    } else "") + moves.map(_.piotr) mkString " "

}
