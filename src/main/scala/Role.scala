package strategygames

sealed trait Role {
  val forsyth: Char
  // draughts.Role.pdn will be referred to by pgn from this point
  val pgn: Char
  val name: String
  val groundName: String
  val binaryInt: Int
  val hashInt: Int
  val storable: Boolean
  def toString(): String
}

sealed trait PromotableRole extends Role {
  // TODO: These functions lie, but we'll have to get over it until
  //       we find the right pattern
  def toChess: chess.PromotableRole
  def toDraughts: draughts.PromotableRole
  def toFairySF: fairysf.PromotableRole
  def toMancala: mancala.PromotableRole
}

object Role {

  final case class ChessRole(r: chess.Role) extends Role {
    val forsyth             = r.forsyth
    val pgn                 = r.pgn
    val binaryInt           = r.binaryInt
    val hashInt             = r.hashInt
    val name                = r.name
    val groundName          = r.groundName
    val storable            = r.storable
    override def toString() = r.name
  }

  final case class DraughtsRole(r: draughts.Role) extends Role {
    lazy val forsyth        = r.forsyth
    lazy val pgn            = r.pdn
    lazy val binaryInt      = r.binaryInt
    lazy val hashInt        = r.hashInt
    lazy val name           = r.name
    lazy val groundName     = r.name
    lazy val storable       = false
    override def toString() = r.name
  }

  final case class FairySFRole(r: fairysf.Role) extends Role {
    lazy val forsyth        = r.forsyth
    lazy val pgn            = r.pgn
    lazy val binaryInt      = r.binaryInt
    lazy val hashInt        = r.hashInt
    lazy val name           = r.name
    lazy val groundName     = r.groundName
    lazy val storable       = r.storable
    override def toString() = r.name
  }

  final case class MancalaRole(r: mancala.Role) extends Role {
    lazy val forsyth        = r.forsyth
    lazy val pgn            = r.pgn
    lazy val binaryInt      = r.binaryInt
    lazy val hashInt        = r.hashInt
    lazy val name           = r.name
    lazy val groundName     = r.groundName
    lazy val storable       = r.storable
    override def toString() = r.name
  }

  final case class ChessPromotableRole(r: chess.PromotableRole) extends PromotableRole {
    lazy val forsyth                        = r.forsyth
    lazy val pgn                            = r.pgn
    lazy val binaryInt                      = r.binaryInt
    lazy val hashInt                        = r.hashInt
    lazy val name                           = r.name
    lazy val groundName                     = r.groundName
    lazy val storable                       = r.storable
    override def toString()                 = r.name
    def toChess                             = r
    def toDraughts: draughts.PromotableRole = sys.error("Not implemented for chess")
    def toFairySF: fairysf.PromotableRole   = sys.error("Not implemented for chess")
    def toMancala: mancala.PromotableRole   = sys.error("Not implemented for chess")
  }

  final case class DraughtsPromotableRole(r: draughts.PromotableRole) extends PromotableRole {
    lazy val forsyth                      = r.forsyth
    lazy val pgn                          = r.pdn
    lazy val binaryInt                    = r.binaryInt
    lazy val hashInt                      = r.hashInt
    lazy val name                         = r.name
    lazy val groundName                   = r.name
    lazy val storable                     = false
    override def toString()               = r.name
    def toDraughts                        = r
    def toChess: chess.PromotableRole     = sys.error("Not implemented for draughts")
    def toFairySF: fairysf.PromotableRole = sys.error("Not implemented for draughts")
    def toMancala: mancala.PromotableRole = sys.error("Not implemented for draughts")
  }

  final case class FairySFPromotableRole(r: fairysf.PromotableRole) extends PromotableRole {
    lazy val forsyth                        = r.forsyth
    lazy val pgn                            = r.pgn
    lazy val binaryInt                      = r.binaryInt
    lazy val hashInt                        = r.hashInt
    lazy val name                           = r.name
    lazy val groundName                     = r.groundName
    lazy val storable                       = r.storable
    override def toString()                 = r.name
    def toDraughts: draughts.PromotableRole = sys.error("Not implemented for fairysf")
    def toChess: chess.PromotableRole       = sys.error("Not implemented for fairysf")
    def toFairySF                           = r
    def toMancala: mancala.PromotableRole   = sys.error("Not implemented for mancala")
  }

  // lila
  def all(lib: GameLogic): List[Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.all.map(DraughtsRole)
    case GameLogic.Chess()    => chess.Role.all.map(ChessRole)
    case GameLogic.FairySF()  => fairysf.Role.all.map(FairySFRole)
    case GameLogic.Mancala()  => mancala.Role.all.map(MancalaRole)
  }

  def allPromotable(lib: GameLogic): List[PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.allPromotable.map(DraughtsPromotableRole)
    case GameLogic.Chess()    => chess.Role.allPromotable.map(ChessPromotableRole)
    case GameLogic.FairySF()  => fairysf.Role.allPromotable.map(FairySFPromotableRole)
    case GameLogic.Mancala()  => sys.error("allPromotable not implemented for mancala")
  }

  def allByForsyth(lib: GameLogic): Map[Char, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByForsyth.map { case (f, r) => (f, DraughtsRole(r)) }
    case GameLogic.Chess()    => chess.Role.allByForsyth.map { case (f, r) => (f, ChessRole(r)) }
    case GameLogic.FairySF()  => fairysf.Role.allByForsyth.map { case (f, r) => (f, FairySFRole(r)) }
    case GameLogic.Mancala()  => mancala.Role.allByForsyth.map { case (f, r) => (f, MancalaRole(r)) }
  }

  def allByForsyth(lib: GameLogic, gf: GameFamily): Map[Char, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByForsyth.map { case (f, r) => (f, DraughtsRole(r)) }
    case GameLogic.Chess()    => chess.Role.allByForsyth.map { case (f, r) => (f, ChessRole(r)) }
    case GameLogic.FairySF()  => fairysf.Role.allByForsyth(gf).map { case (f, r) => (f, FairySFRole(r)) }
    case GameLogic.Mancala()  => mancala.Role.allByForsyth(gf).map { case (f, r) => (f, MancalaRole(r)) }
  }

  def allByPgn(lib: GameLogic): Map[Char, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByPdn.map { case (p, r) => (p, DraughtsRole(r)) }
    case GameLogic.Chess()    => chess.Role.allByPgn.map { case (p, r) => (p, ChessRole(r)) }
    case GameLogic.FairySF()  => fairysf.Role.allByPgn.map { case (p, r) => (p, FairySFRole(r)) }
    case GameLogic.Mancala()  => mancala.Role.allByPgn.map { case (p, r) => (p, MancalaRole(r)) }
  }

  def allByPgn(lib: GameLogic, gf: GameFamily): Map[Char, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByPdn.map { case (p, r) => (p, DraughtsRole(r)) }
    case GameLogic.Chess()    => chess.Role.allByPgn.map { case (p, r) => (p, ChessRole(r)) }
    case GameLogic.FairySF()  => fairysf.Role.allByPgn(gf).map { case (p, r) => (p, FairySFRole(r)) }
    case GameLogic.Mancala()  => mancala.Role.allByPgn(gf).map { case (p, r) => (p, MancalaRole(r)) }
  }

  def allByName(lib: GameLogic): Map[String, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByName.map { case (n, r) => (n, DraughtsRole(r)) }
    case GameLogic.Chess()    => chess.Role.allByName.map { case (n, r) => (n, ChessRole(r)) }
    case GameLogic.FairySF()  => fairysf.Role.allByName.map { case (n, r) => (n, FairySFRole(r)) }
    case GameLogic.Mancala()  => mancala.Role.allByName.map { case (n, r) => (n, MancalaRole(r)) }
  }

  def allByName(lib: GameLogic, gf: GameFamily): Map[String, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByName.map { case (n, r) => (n, DraughtsRole(r)) }
    case GameLogic.Chess()    => chess.Role.allByName.map { case (n, r) => (n, ChessRole(r)) }
    case GameLogic.FairySF()  => fairysf.Role.allByName(gf).map { case (n, r) => (n, FairySFRole(r)) }
    case GameLogic.Mancala()  => mancala.Role.allByName(gf).map { case (n, r) => (n, MancalaRole(r)) }
  }

  def allByGroundName(lib: GameLogic): Map[String, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByName.map { case (n, r) => (n, DraughtsRole(r)) }
    case GameLogic.Chess()    => chess.Role.allByGroundName.map { case (n, r) => (n, ChessRole(r)) }
    case GameLogic.FairySF()  => fairysf.Role.allByGroundName.map { case (n, r) => (n, FairySFRole(r)) }
    case GameLogic.Mancala()  => mancala.Role.allByGroundName.map { case (n, r) => (n, MancalaRole(r)) }
  }

  def allByGroundName(lib: GameLogic, gf: GameFamily): Map[String, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByName.map { case (n, r) => (n, DraughtsRole(r)) }
    case GameLogic.Chess()    => chess.Role.allByGroundName.map { case (n, r) => (n, ChessRole(r)) }
    case GameLogic.FairySF()  => fairysf.Role.allByGroundName(gf).map { case (n, r) => (n, FairySFRole(r)) }
    case GameLogic.Mancala()  => mancala.Role.allByGroundName(gf).map { case (n, r) => (n, MancalaRole(r)) }
  }

  def allPromotableByName(lib: GameLogic): Map[String, PromotableRole] = lib match {
    case GameLogic.Draughts() =>
      draughts.Role.allPromotableByName.map { case (n, r) => (n, DraughtsPromotableRole(r)) }
    case GameLogic.Chess()    =>
      chess.Role.allPromotableByName.map { case (n, r) => (n, ChessPromotableRole(r)) }
    case GameLogic.FairySF()  =>
      fairysf.Role.allPromotableByName.map { case (n, r) => (n, FairySFPromotableRole(r)) }
    case GameLogic.Mancala()  => sys.error("allPromotableByName not implemented for mancala")
  }

  def allPromotableByName(lib: GameLogic, gf: GameFamily): Map[String, PromotableRole] = lib match {
    case GameLogic.Draughts() =>
      draughts.Role.allPromotableByName.map { case (n, r) => (n, DraughtsPromotableRole(r)) }
    case GameLogic.Chess()    =>
      chess.Role.allPromotableByName.map { case (n, r) => (n, ChessPromotableRole(r)) }
    case GameLogic.FairySF()  =>
      fairysf.Role.allPromotableByName(gf).map { case (n, r) => (n, FairySFPromotableRole(r)) }
    case GameLogic.Mancala()  => sys.error("allPromotableByName not implemented for mancala")
  }

  def allPromotableByForsyth(lib: GameLogic): Map[Char, PromotableRole] = lib match {
    case GameLogic.Draughts() =>
      draughts.Role.allPromotableByForsyth.map { case (f, r) => (f, DraughtsPromotableRole(r)) }
    case GameLogic.Chess()    =>
      chess.Role.allPromotableByForsyth.map { case (f, r) => (f, ChessPromotableRole(r)) }
    case GameLogic.FairySF()  =>
      fairysf.Role.allPromotableByForsyth.map { case (f, r) => (f, FairySFPromotableRole(r)) }
    case GameLogic.Mancala()  => sys.error("allPromotableByForsyth not implemented for mancala")
  }

  def allPromotableByForsyth(lib: GameLogic, gf: GameFamily): Map[Char, PromotableRole] = lib match {
    case GameLogic.Draughts() =>
      draughts.Role.allPromotableByForsyth.map { case (f, r) => (f, DraughtsPromotableRole(r)) }
    case GameLogic.Chess()    =>
      chess.Role.allPromotableByForsyth.map { case (f, r) => (f, ChessPromotableRole(r)) }
    case GameLogic.FairySF()  =>
      fairysf.Role.allPromotableByForsyth(gf).map { case (f, r) => (f, FairySFPromotableRole(r)) }
    case GameLogic.Mancala()  => sys.error("allPromotableByForsyth not implemented for mancala")
  }

  def allPromotableByPgn(lib: GameLogic): Map[Char, PromotableRole] = lib match {
    case GameLogic.Draughts() =>
      draughts.Role.allPromotableByPdn.map { case (p, r) => (p, DraughtsPromotableRole(r)) }
    case GameLogic.Chess()    => chess.Role.allPromotableByPgn.map { case (p, r) => (p, ChessPromotableRole(r)) }
    case GameLogic.FairySF()  =>
      fairysf.Role.allPromotableByPgn.map { case (p, r) => (p, FairySFPromotableRole(r)) }
    case GameLogic.Mancala()  => sys.error("allPromotableByPgn not implemented for mancala")
  }

  def allPromotableByPgn(lib: GameLogic, gf: GameFamily): Map[Char, PromotableRole] = lib match {
    case GameLogic.Draughts() =>
      draughts.Role.allPromotableByPdn.map { case (p, r) => (p, DraughtsPromotableRole(r)) }
    case GameLogic.Chess()    => chess.Role.allPromotableByPgn.map { case (p, r) => (p, ChessPromotableRole(r)) }
    case GameLogic.FairySF()  =>
      fairysf.Role.allPromotableByPgn(gf).map { case (p, r) => (p, FairySFPromotableRole(r)) }
    case GameLogic.Mancala()  => sys.error("allPromotableByPgn not implemented for mancala")
  }

  def forsyth(lib: GameLogic, c: Char): Option[Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.forsyth(c).map(DraughtsRole)
    case GameLogic.Chess()    => chess.Role.forsyth(c).map(ChessRole)
    case GameLogic.FairySF()  => fairysf.Role.forsyth(c).map(FairySFRole)
    case GameLogic.Mancala()  => mancala.Role.forsyth(c).map(MancalaRole)
  }

  def promotable(lib: GameLogic, gf: GameFamily, c: Char): Option[PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.promotable(c).map(DraughtsPromotableRole)
    case GameLogic.Chess()    => chess.Role.promotable(c).map(ChessPromotableRole)
    case GameLogic.FairySF()  => fairysf.Role.promotable(gf, c).map(FairySFPromotableRole)
    case GameLogic.Mancala()  => sys.error("promotable not implemented for mancala")
  }

  def promotable(lib: GameLogic, gf: GameFamily, name: String): Option[PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.promotable(name).map(DraughtsPromotableRole)
    case GameLogic.Chess()    => chess.Role.promotable(name).map(ChessPromotableRole)
    case GameLogic.FairySF()  => fairysf.Role.promotable(gf, name).map(FairySFPromotableRole)
    case GameLogic.Mancala()  => sys.error("promotable not implemented for mancala")
  }

  def promotable(lib: GameLogic, gf: GameFamily, name: Option[String]): Option[PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.promotable(name).map(DraughtsPromotableRole)
    case GameLogic.Chess()    => chess.Role.promotable(name).map(ChessPromotableRole)
    case GameLogic.FairySF()  => fairysf.Role.promotable(gf, name).map(FairySFPromotableRole)
    case GameLogic.Mancala()  => sys.error("promotable not implemented for mancala")
  }

  def storable(lib: GameLogic): List[Role] = lib match {
    case GameLogic.Draughts() => List()
    case GameLogic.Chess()    => chess.Role.storable.map(ChessRole)
    case GameLogic.FairySF()  => fairysf.Role.storable.map(FairySFRole)
    case GameLogic.Mancala()  => mancala.Role.storable.map(MancalaRole)
  }

  def pgnMoveToRole(lib: GameLogic, gf: GameFamily, c: Char): Role = lib match {
    case GameLogic.Draughts() => DraughtsRole(draughts.Role.pdnMoveToRole(c))
    case GameLogic.Chess()    => ChessRole(chess.Role.pgnMoveToRole(c))
    case GameLogic.FairySF()  => FairySFRole(fairysf.Role.pgnMoveToRole(gf, c))
    case GameLogic.Mancala()  => MancalaRole(mancala.Role.pgnMoveToRole(gf, c))
  }

  def javaSymbolToRole(lib: GameLogic, s: String): Role = lib match {
    case GameLogic.Draughts() => DraughtsRole(draughts.Role.javaSymbolToRole(s))
    case GameLogic.Chess()    => ChessRole(chess.Role.javaSymbolToRole(s))
    case GameLogic.FairySF()  => FairySFRole(fairysf.Role.javaSymbolToRole(s))
    case GameLogic.Mancala()  => MancalaRole(mancala.Role.javaSymbolToRole(s))
  }

  def wrap(pr: chess.PromotableRole): PromotableRole    = ChessPromotableRole(pr)
  def wrap(pr: draughts.PromotableRole): PromotableRole = DraughtsPromotableRole(pr)
  def wrap(pr: fairysf.PromotableRole): PromotableRole  = FairySFPromotableRole(pr)

}
