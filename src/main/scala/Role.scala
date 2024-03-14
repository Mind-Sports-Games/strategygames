package strategygames

sealed trait Role {
  val gameLogic: GameLogic
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
  def toSamurai: samurai.PromotableRole
  def toTogyzkumalak: togyzkumalak.PromotableRole
  def toGo: go.PromotableRole
  def toBackgammon: backgammon.PromotableRole
}

object Role {

  final case class ChessRole(r: chess.Role) extends Role {
    val gameLogic           = GameLogic.Chess()
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
    lazy val gameLogic      = GameLogic.Draughts()
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
    lazy val gameLogic      = GameLogic.FairySF()
    lazy val forsyth        = r.forsyth
    lazy val pgn            = r.pgn
    lazy val binaryInt      = r.binaryInt
    lazy val hashInt        = r.hashInt
    lazy val name           = r.name
    lazy val groundName     = r.groundName
    lazy val storable       = r.storable
    override def toString() = r.name
  }

  final case class SamuraiRole(r: samurai.Role) extends Role {
    lazy val gameLogic      = GameLogic.Samurai()
    lazy val forsyth        = r.forsyth
    lazy val pgn            = r.pgn
    lazy val binaryInt      = r.binaryInt
    lazy val hashInt        = r.hashInt
    lazy val name           = r.name
    lazy val groundName     = r.groundName
    lazy val storable       = false
    override def toString() = r.name
  }

  final case class TogyzkumalakRole(r: togyzkumalak.Role) extends Role {
    lazy val gameLogic      = GameLogic.Togyzkumalak()
    lazy val forsyth        = r.forsyth
    lazy val pgn            = r.pgn
    lazy val binaryInt      = r.binaryInt
    lazy val hashInt        = r.hashInt
    lazy val name           = r.name
    lazy val groundName     = r.groundName
    lazy val storable       = false
    override def toString() = r.name
  }

  final case class GoRole(r: go.Role) extends Role {
    lazy val gameLogic      = GameLogic.Go()
    lazy val forsyth        = r.forsyth
    lazy val pgn            = r.pgn
    lazy val binaryInt      = r.binaryInt
    lazy val hashInt        = r.hashInt
    lazy val name           = r.name
    lazy val groundName     = r.groundName
    lazy val storable       = false
    override def toString() = r.name
  }

  final case class BackgammonRole(r: backgammon.Role) extends Role {
    lazy val gameLogic      = GameLogic.Backgammon()
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
    lazy val gameLogic                              = GameLogic.Chess()
    lazy val forsyth                                = r.forsyth
    lazy val pgn                                    = r.pgn
    lazy val binaryInt                              = r.binaryInt
    lazy val hashInt                                = r.hashInt
    lazy val name                                   = r.name
    lazy val groundName                             = r.groundName
    lazy val storable                               = r.storable
    override def toString()                         = r.name
    def toChess                                     = r
    def toDraughts: draughts.PromotableRole         = sys.error("Not implemented for chess")
    def toFairySF: fairysf.PromotableRole           = sys.error("Not implemented for chess")
    def toSamurai: samurai.PromotableRole           = sys.error("Not implemented for chess")
    def toTogyzkumalak: togyzkumalak.PromotableRole = sys.error("Not implemented for chess")
    def toGo: go.PromotableRole                     = sys.error("Not implemented for chess")
    def toBackgammon: backgammon.PromotableRole     = sys.error("Not implemented for chess")
  }

  final case class DraughtsPromotableRole(r: draughts.PromotableRole) extends PromotableRole {
    lazy val gameLogic                              = GameLogic.Draughts()
    lazy val forsyth                                = r.forsyth
    lazy val pgn                                    = r.pdn
    lazy val binaryInt                              = r.binaryInt
    lazy val hashInt                                = r.hashInt
    lazy val name                                   = r.name
    lazy val groundName                             = r.name
    lazy val storable                               = false
    override def toString()                         = r.name
    def toDraughts                                  = r
    def toChess: chess.PromotableRole               = sys.error("Not implemented for draughts")
    def toFairySF: fairysf.PromotableRole           = sys.error("Not implemented for draughts")
    def toSamurai: samurai.PromotableRole           = sys.error("Not implemented for draughts")
    def toTogyzkumalak: togyzkumalak.PromotableRole = sys.error("Not implemented for draughts")
    def toGo: go.PromotableRole                     = sys.error("Not implemented for draughts")
    def toBackgammon: backgammon.PromotableRole     = sys.error("Not implemented for draughts")
  }

  final case class FairySFPromotableRole(r: fairysf.PromotableRole) extends PromotableRole {
    lazy val gameLogic                              = GameLogic.FairySF()
    lazy val forsyth                                = r.forsyth
    lazy val pgn                                    = r.pgn
    lazy val binaryInt                              = r.binaryInt
    lazy val hashInt                                = r.hashInt
    lazy val name                                   = r.name
    lazy val groundName                             = r.groundName
    lazy val storable                               = r.storable
    override def toString()                         = r.name
    def toDraughts: draughts.PromotableRole         = sys.error("Not implemented for fairysf")
    def toChess: chess.PromotableRole               = sys.error("Not implemented for fairysf")
    def toFairySF                                   = r
    def toSamurai: samurai.PromotableRole           = sys.error("Not implemented for samurai")
    def toTogyzkumalak: togyzkumalak.PromotableRole = sys.error("Not implemented for togyzkumalak")
    def toGo: go.PromotableRole                     = sys.error("Not implemented for go")
    def toBackgammon: backgammon.PromotableRole     = sys.error("Not implemented for backgammon")
  }

  // lila
  def all(lib: GameLogic): List[Role] = lib match {
    case GameLogic.Draughts()     => draughts.Role.all.map(DraughtsRole)
    case GameLogic.Chess()        => chess.Role.all.map(ChessRole)
    case GameLogic.FairySF()      => fairysf.Role.all.map(FairySFRole)
    case GameLogic.Samurai()      => samurai.Role.all.map(SamuraiRole)
    case GameLogic.Togyzkumalak() => togyzkumalak.Role.all.map(TogyzkumalakRole)
    case GameLogic.Go()           => go.Role.all.map(GoRole)
    case GameLogic.Backgammon()   => backgammon.Role.all.map(BackgammonRole)
  }

  def allPromotable(lib: GameLogic): List[PromotableRole] = lib match {
    case GameLogic.Draughts()     => draughts.Role.allPromotable.map(DraughtsPromotableRole)
    case GameLogic.Chess()        => chess.Role.allPromotable.map(ChessPromotableRole)
    case GameLogic.FairySF()      => fairysf.Role.allPromotable.map(FairySFPromotableRole)
    case GameLogic.Samurai()      => sys.error("allPromotable not implemented for samurai")
    case GameLogic.Togyzkumalak() => sys.error("allPromotable not implemented for togyzkumalak")
    case GameLogic.Go()           => sys.error("allPromotable not implemented for go")
    case GameLogic.Backgammon()   => sys.error("allPromotable not implemented for backgammon")
  }

  def allByForsyth(lib: GameLogic): Map[Char, Role] = lib match {
    case GameLogic.Draughts()     => draughts.Role.allByForsyth.map { case (f, r) => (f, DraughtsRole(r)) }
    case GameLogic.Chess()        => chess.Role.allByForsyth.map { case (f, r) => (f, ChessRole(r)) }
    case GameLogic.FairySF()      => fairysf.Role.allByForsyth.map { case (f, r) => (f, FairySFRole(r)) }
    case GameLogic.Samurai()      => samurai.Role.allByForsyth.map { case (f, r) => (f, SamuraiRole(r)) }
    case GameLogic.Togyzkumalak() =>
      togyzkumalak.Role.allByForsyth.map { case (f, r) => (f, TogyzkumalakRole(r)) }
    case GameLogic.Go()           => go.Role.allByForsyth.map { case (f, r) => (f, GoRole(r)) }
    case GameLogic.Backgammon()   => backgammon.Role.allByForsyth.map { case (f, r) => (f, BackgammonRole(r)) }
  }

  def allByForsyth(lib: GameLogic, gf: GameFamily): Map[Char, Role] = lib match {
    case GameLogic.Draughts()     => draughts.Role.allByForsyth.map { case (f, r) => (f, DraughtsRole(r)) }
    case GameLogic.Chess()        => chess.Role.allByForsyth.map { case (f, r) => (f, ChessRole(r)) }
    case GameLogic.FairySF()      => fairysf.Role.allByForsyth(gf).map { case (f, r) => (f, FairySFRole(r)) }
    case GameLogic.Samurai()      => samurai.Role.allByForsyth(gf).map { case (f, r) => (f, SamuraiRole(r)) }
    case GameLogic.Togyzkumalak() =>
      togyzkumalak.Role.allByForsyth(gf).map { case (f, r) => (f, TogyzkumalakRole(r)) }
    case GameLogic.Go()           => go.Role.allByForsyth(gf).map { case (f, r) => (f, GoRole(r)) }
    case GameLogic.Backgammon()   =>
      backgammon.Role.allByForsyth(gf).map { case (f, r) => (f, BackgammonRole(r)) }
  }

  def allByPgn(lib: GameLogic): Map[Char, Role] = lib match {
    case GameLogic.Draughts()     => draughts.Role.allByPdn.map { case (p, r) => (p, DraughtsRole(r)) }
    case GameLogic.Chess()        => chess.Role.allByPgn.map { case (p, r) => (p, ChessRole(r)) }
    case GameLogic.FairySF()      => fairysf.Role.allByPgn.map { case (p, r) => (p, FairySFRole(r)) }
    case GameLogic.Samurai()      => samurai.Role.allByPgn.map { case (p, r) => (p, SamuraiRole(r)) }
    case GameLogic.Togyzkumalak() =>
      togyzkumalak.Role.allByPgn.map { case (p, r) => (p, TogyzkumalakRole(r)) }
    case GameLogic.Go()           => go.Role.allByPgn.map { case (p, r) => (p, GoRole(r)) }
    case GameLogic.Backgammon()   => backgammon.Role.allByPgn.map { case (p, r) => (p, BackgammonRole(r)) }
  }

  def allByPgn(lib: GameLogic, gf: GameFamily): Map[Char, Role] = lib match {
    case GameLogic.Draughts()     => draughts.Role.allByPdn.map { case (p, r) => (p, DraughtsRole(r)) }
    case GameLogic.Chess()        => chess.Role.allByPgn.map { case (p, r) => (p, ChessRole(r)) }
    case GameLogic.FairySF()      => fairysf.Role.allByPgn(gf).map { case (p, r) => (p, FairySFRole(r)) }
    case GameLogic.Samurai()      => samurai.Role.allByPgn(gf).map { case (p, r) => (p, SamuraiRole(r)) }
    case GameLogic.Togyzkumalak() =>
      togyzkumalak.Role.allByPgn(gf).map { case (p, r) => (p, TogyzkumalakRole(r)) }
    case GameLogic.Go()           => go.Role.allByPgn(gf).map { case (p, r) => (p, GoRole(r)) }
    case GameLogic.Backgammon()   => backgammon.Role.allByPgn(gf).map { case (p, r) => (p, BackgammonRole(r)) }
  }

  def allByName(lib: GameLogic): Map[String, Role] = lib match {
    case GameLogic.Draughts()     => draughts.Role.allByName.map { case (n, r) => (n, DraughtsRole(r)) }
    case GameLogic.Chess()        => chess.Role.allByName.map { case (n, r) => (n, ChessRole(r)) }
    case GameLogic.FairySF()      => fairysf.Role.allByName.map { case (n, r) => (n, FairySFRole(r)) }
    case GameLogic.Samurai()      => samurai.Role.allByName.map { case (n, r) => (n, SamuraiRole(r)) }
    case GameLogic.Togyzkumalak() =>
      togyzkumalak.Role.allByName.map { case (n, r) => (n, TogyzkumalakRole(r)) }
    case GameLogic.Go()           => go.Role.allByName.map { case (n, r) => (n, GoRole(r)) }
    case GameLogic.Backgammon()   => backgammon.Role.allByName.map { case (n, r) => (n, BackgammonRole(r)) }
  }

  def allByName(lib: GameLogic, gf: GameFamily): Map[String, Role] = lib match {
    case GameLogic.Draughts()     => draughts.Role.allByName.map { case (n, r) => (n, DraughtsRole(r)) }
    case GameLogic.Chess()        => chess.Role.allByName.map { case (n, r) => (n, ChessRole(r)) }
    case GameLogic.FairySF()      => fairysf.Role.allByName(gf).map { case (n, r) => (n, FairySFRole(r)) }
    case GameLogic.Samurai()      => samurai.Role.allByName(gf).map { case (n, r) => (n, SamuraiRole(r)) }
    case GameLogic.Togyzkumalak() =>
      togyzkumalak.Role.allByName(gf).map { case (n, r) => (n, TogyzkumalakRole(r)) }
    case GameLogic.Go()           => go.Role.allByName(gf).map { case (n, r) => (n, GoRole(r)) }
    case GameLogic.Backgammon()   => backgammon.Role.allByName(gf).map { case (n, r) => (n, BackgammonRole(r)) }
  }

  def allByGroundName(lib: GameLogic): Map[String, Role] = lib match {
    case GameLogic.Draughts()     => draughts.Role.allByName.map { case (n, r) => (n, DraughtsRole(r)) }
    case GameLogic.Chess()        => chess.Role.allByGroundName.map { case (n, r) => (n, ChessRole(r)) }
    case GameLogic.FairySF()      => fairysf.Role.allByGroundName.map { case (n, r) => (n, FairySFRole(r)) }
    case GameLogic.Samurai()      => samurai.Role.allByGroundName.map { case (n, r) => (n, SamuraiRole(r)) }
    case GameLogic.Togyzkumalak() =>
      togyzkumalak.Role.allByGroundName.map { case (n, r) => (n, TogyzkumalakRole(r)) }
    case GameLogic.Go()           => go.Role.allByGroundName.map { case (n, r) => (n, GoRole(r)) }
    case GameLogic.Backgammon()   =>
      backgammon.Role.allByGroundName.map { case (n, r) => (n, BackgammonRole(r)) }
  }

  def allByGroundName(lib: GameLogic, gf: GameFamily): Map[String, Role] = lib match {
    case GameLogic.Draughts()     => draughts.Role.allByName.map { case (n, r) => (n, DraughtsRole(r)) }
    case GameLogic.Chess()        => chess.Role.allByGroundName.map { case (n, r) => (n, ChessRole(r)) }
    case GameLogic.FairySF()      => fairysf.Role.allByGroundName(gf).map { case (n, r) => (n, FairySFRole(r)) }
    case GameLogic.Samurai()      => samurai.Role.allByGroundName(gf).map { case (n, r) => (n, SamuraiRole(r)) }
    case GameLogic.Togyzkumalak() =>
      togyzkumalak.Role.allByGroundName(gf).map { case (n, r) => (n, TogyzkumalakRole(r)) }
    case GameLogic.Go()           => go.Role.allByGroundName(gf).map { case (n, r) => (n, GoRole(r)) }
    case GameLogic.Backgammon()   =>
      backgammon.Role.allByGroundName(gf).map { case (n, r) => (n, BackgammonRole(r)) }
  }

  def allPromotableByName(lib: GameLogic): Map[String, PromotableRole] = lib match {
    case GameLogic.Draughts()     =>
      draughts.Role.allPromotableByName.map { case (n, r) => (n, DraughtsPromotableRole(r)) }
    case GameLogic.Chess()        =>
      chess.Role.allPromotableByName.map { case (n, r) => (n, ChessPromotableRole(r)) }
    case GameLogic.FairySF()      =>
      fairysf.Role.allPromotableByName.map { case (n, r) => (n, FairySFPromotableRole(r)) }
    case GameLogic.Samurai()      => sys.error("allPromotableByName not implemented for samurai")
    case GameLogic.Togyzkumalak() => sys.error("allPromotableByName not implemented for togyzkumalak")
    case GameLogic.Go()           => sys.error("allPromotableByName not implemented for go")
    case GameLogic.Backgammon()   => sys.error("allPromotableByName not implemented for backgammon")
  }

  def allPromotableByName(lib: GameLogic, gf: GameFamily): Map[String, PromotableRole] = lib match {
    case GameLogic.Draughts()     =>
      draughts.Role.allPromotableByName.map { case (n, r) => (n, DraughtsPromotableRole(r)) }
    case GameLogic.Chess()        =>
      chess.Role.allPromotableByName.map { case (n, r) => (n, ChessPromotableRole(r)) }
    case GameLogic.FairySF()      =>
      fairysf.Role.allPromotableByName(gf).map { case (n, r) => (n, FairySFPromotableRole(r)) }
    case GameLogic.Samurai()      => sys.error("allPromotableByName not implemented for samurai")
    case GameLogic.Togyzkumalak() => sys.error("allPromotableByName not implemented for togyzkumalak")
    case GameLogic.Go()           => sys.error("allPromotableByName not implemented for go")
    case GameLogic.Backgammon()   => sys.error("allPromotableByName not implemented for backgammon")
  }

  def allPromotableByGroundName(lib: GameLogic): Map[String, PromotableRole] = lib match {
    case GameLogic.Chess()        =>
      chess.Role.allPromotableByGroundName.map { case (n, r) => (n, ChessPromotableRole(r)) }
    case GameLogic.FairySF()      =>
      fairysf.Role.allPromotableByGroundName.map { case (n, r) => (n, FairySFPromotableRole(r)) }
    case GameLogic.Draughts()     => sys.error("allPromotableByGroundName not implemented for draughts")
    case GameLogic.Samurai()      => sys.error("allPromotableByGroundName not implemented for samurai")
    case GameLogic.Togyzkumalak() => sys.error("allPromotableByGroundName not implemented for togyzkumalak")
    case GameLogic.Go()           => sys.error("allPromotableByGroundName not implemented for go")
  }

  def allPromotableByGroundName(lib: GameLogic, gf: GameFamily): Map[String, PromotableRole] = lib match {
    case GameLogic.Chess()        =>
      chess.Role.allPromotableByGroundName.map { case (n, r) => (n, ChessPromotableRole(r)) }
    case GameLogic.FairySF()      =>
      fairysf.Role.allPromotableByGroundName(gf).map { case (n, r) => (n, FairySFPromotableRole(r)) }
    case GameLogic.Draughts()     => sys.error("allPromotableByGroundName not implemented for draughts")
    case GameLogic.Samurai()      => sys.error("allPromotableByGroundName not implemented for samurai")
    case GameLogic.Togyzkumalak() => sys.error("allPromotableByGroundName not implemented for togyzkumalak")
    case GameLogic.Go()           => sys.error("allPromotableByGroundName not implemented for go")
  }

  def allPromotableByForsyth(lib: GameLogic): Map[Char, PromotableRole] = lib match {
    case GameLogic.Draughts()     =>
      draughts.Role.allPromotableByForsyth.map { case (f, r) => (f, DraughtsPromotableRole(r)) }
    case GameLogic.Chess()        =>
      chess.Role.allPromotableByForsyth.map { case (f, r) => (f, ChessPromotableRole(r)) }
    case GameLogic.FairySF()      =>
      fairysf.Role.allPromotableByForsyth.map { case (f, r) => (f, FairySFPromotableRole(r)) }
    case GameLogic.Samurai()      => sys.error("allPromotableByForsyth not implemented for samurai")
    case GameLogic.Togyzkumalak() => sys.error("allPromotableByForsyth not implemented for togyzkumalak")
    case GameLogic.Go()           => sys.error("allPromotableByForsyth not implemented for go")
    case GameLogic.Backgammon()   => sys.error("allPromotableByForsyth not implemented for backgammon")
  }

  def allPromotableByForsyth(lib: GameLogic, gf: GameFamily): Map[Char, PromotableRole] = lib match {
    case GameLogic.Draughts()     =>
      draughts.Role.allPromotableByForsyth.map { case (f, r) => (f, DraughtsPromotableRole(r)) }
    case GameLogic.Chess()        =>
      chess.Role.allPromotableByForsyth.map { case (f, r) => (f, ChessPromotableRole(r)) }
    case GameLogic.FairySF()      =>
      fairysf.Role.allPromotableByForsyth(gf).map { case (f, r) => (f, FairySFPromotableRole(r)) }
    case GameLogic.Samurai()      => sys.error("allPromotableByForsyth not implemented for samurai")
    case GameLogic.Togyzkumalak() => sys.error("allPromotableByForsyth not implemented for togyzkumalak")
    case GameLogic.Go()           => sys.error("allPromotableByForsyth not implemented for go")
    case GameLogic.Backgammon()   => sys.error("allPromotableByForsyth not implemented for backgammon")
  }

  def allPromotableByPgn(lib: GameLogic): Map[Char, PromotableRole] = lib match {
    case GameLogic.Draughts()     =>
      draughts.Role.allPromotableByPdn.map { case (p, r) => (p, DraughtsPromotableRole(r)) }
    case GameLogic.Chess()        => chess.Role.allPromotableByPgn.map { case (p, r) => (p, ChessPromotableRole(r)) }
    case GameLogic.FairySF()      =>
      fairysf.Role.allPromotableByPgn.map { case (p, r) => (p, FairySFPromotableRole(r)) }
    case GameLogic.Samurai()      => sys.error("allPromotableByPgn not implemented for samurai")
    case GameLogic.Togyzkumalak() => sys.error("allPromotableByPgn not implemented for togyzkumalak")
    case GameLogic.Go()           => sys.error("allPromotableByPgn not implemented for go")
    case GameLogic.Backgammon()   => sys.error("allPromotableByPgn not implemented for backgammon")
  }

  def allPromotableByPgn(lib: GameLogic, gf: GameFamily): Map[Char, PromotableRole] = lib match {
    case GameLogic.Draughts()     =>
      draughts.Role.allPromotableByPdn.map { case (p, r) => (p, DraughtsPromotableRole(r)) }
    case GameLogic.Chess()        => chess.Role.allPromotableByPgn.map { case (p, r) => (p, ChessPromotableRole(r)) }
    case GameLogic.FairySF()      =>
      fairysf.Role.allPromotableByPgn(gf).map { case (p, r) => (p, FairySFPromotableRole(r)) }
    case GameLogic.Samurai()      => sys.error("allPromotableByPgn not implemented for samurai")
    case GameLogic.Togyzkumalak() => sys.error("allPromotableByPgn not implemented for togyzkumalak")
    case GameLogic.Go()           => sys.error("allPromotableByPgn not implemented for go")
    case GameLogic.Backgammon()   => sys.error("allPromotableByPgn not implemented for backgammon")
  }

  def forsyth(lib: GameLogic, c: Char): Option[Role] = lib match {
    case GameLogic.Draughts()     => draughts.Role.forsyth(c).map(DraughtsRole)
    case GameLogic.Chess()        => chess.Role.forsyth(c).map(ChessRole)
    case GameLogic.FairySF()      => fairysf.Role.forsyth(c).map(FairySFRole)
    case GameLogic.Samurai()      => samurai.Role.forsyth(c).map(SamuraiRole)
    case GameLogic.Togyzkumalak() => togyzkumalak.Role.forsyth(c).map(TogyzkumalakRole)
    case GameLogic.Go()           => go.Role.forsyth(c).map(GoRole)
    case GameLogic.Backgammon()   => backgammon.Role.forsyth(c).map(BackgammonRole)
  }

  def promotable(lib: GameLogic, gf: GameFamily, c: Char): Option[PromotableRole] = lib match {
    case GameLogic.Draughts()     => draughts.Role.promotable(c).map(DraughtsPromotableRole)
    case GameLogic.Chess()        => chess.Role.promotable(c).map(ChessPromotableRole)
    case GameLogic.FairySF()      => fairysf.Role.promotable(gf, c).map(FairySFPromotableRole)
    case GameLogic.Samurai()      => sys.error("promotable not implemented for samurai")
    case GameLogic.Togyzkumalak() => sys.error("promotable not implemented for togyzkumalak")
    case GameLogic.Go()           => sys.error("promotable not implemented for go")
    case GameLogic.Backgammon()   => sys.error("promotable not implemented for backgammon")
  }

  def promotable(lib: GameLogic, gf: GameFamily, name: String): Option[PromotableRole] = lib match {
    case GameLogic.Draughts()     => draughts.Role.promotable(name).map(DraughtsPromotableRole)
    case GameLogic.Chess()        => chess.Role.promotable(name).map(ChessPromotableRole)
    case GameLogic.FairySF()      => fairysf.Role.promotable(gf, name).map(FairySFPromotableRole)
    case GameLogic.Samurai()      => sys.error("promotable not implemented for samurai")
    case GameLogic.Togyzkumalak() => sys.error("promotable not implemented for togyzkumalak")
    case GameLogic.Go()           => sys.error("promotable not implemented for go")
    case GameLogic.Backgammon()   => sys.error("promotable not implemented for backgammon")
  }

  def promotable(lib: GameLogic, gf: GameFamily, name: Option[String]): Option[PromotableRole] = lib match {
    case GameLogic.Draughts()     => draughts.Role.promotable(name).map(DraughtsPromotableRole)
    case GameLogic.Chess()        => chess.Role.promotable(name).map(ChessPromotableRole)
    case GameLogic.FairySF()      => fairysf.Role.promotable(gf, name).map(FairySFPromotableRole)
    case GameLogic.Samurai()      => sys.error("promotable not implemented for samurai")
    case GameLogic.Togyzkumalak() => sys.error("promotable not implemented for togyzkumalak")
    case GameLogic.Go()           => sys.error("promotable not implemented for go")
    case GameLogic.Backgammon()   => sys.error("promotable not implemented for backgammon")
  }

  def storable(lib: GameLogic): List[Role] = lib match {
    case GameLogic.Draughts()     => List()
    case GameLogic.Chess()        => chess.Role.storable.map(ChessRole)
    case GameLogic.FairySF()      => fairysf.Role.storable.map(FairySFRole)
    case GameLogic.Samurai()      => List()
    case GameLogic.Togyzkumalak() => List()
    case GameLogic.Go()           => List()
    case GameLogic.Backgammon()   => backgammon.Role.storable.map(BackgammonRole)
  }

  def pgnMoveToRole(lib: GameLogic, gf: GameFamily, c: Char): Role = lib match {
    case GameLogic.Draughts()     => DraughtsRole(draughts.Role.pdnMoveToRole(c))
    case GameLogic.Chess()        => ChessRole(chess.Role.pgnMoveToRole(c))
    case GameLogic.FairySF()      => FairySFRole(fairysf.Role.pgnMoveToRole(gf, c))
    case GameLogic.Samurai()      => SamuraiRole(samurai.Role.pgnMoveToRole(gf, c))
    case GameLogic.Togyzkumalak() => TogyzkumalakRole(togyzkumalak.Role.pgnMoveToRole(gf, c))
    case GameLogic.Go()           => GoRole(go.Role.pgnMoveToRole(gf, c))
    case GameLogic.Backgammon()   => BackgammonRole(backgammon.Role.pgnMoveToRole(gf, c))
  }

  def javaSymbolToRole(lib: GameLogic, s: String): Role = lib match {
    case GameLogic.Draughts()     => DraughtsRole(draughts.Role.javaSymbolToRole(s))
    case GameLogic.Chess()        => ChessRole(chess.Role.javaSymbolToRole(s))
    case GameLogic.FairySF()      => FairySFRole(fairysf.Role.javaSymbolToRole(s))
    case GameLogic.Samurai()      => SamuraiRole(samurai.Role.javaSymbolToRole(s))
    case GameLogic.Togyzkumalak() => TogyzkumalakRole(togyzkumalak.Role.javaSymbolToRole(s))
    case GameLogic.Go()           => GoRole(go.Role.javaSymbolToRole(s))
    case GameLogic.Backgammon()   => BackgammonRole(backgammon.Role.javaSymbolToRole(s))
  }

  def wrap(pr: chess.PromotableRole): PromotableRole    = ChessPromotableRole(pr)
  def wrap(pr: draughts.PromotableRole): PromotableRole = DraughtsPromotableRole(pr)
  def wrap(pr: fairysf.PromotableRole): PromotableRole  = FairySFPromotableRole(pr)

}
