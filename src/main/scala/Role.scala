package strategygames

sealed trait Role {
  val forsyth: Char
  //draughts.Role.pdn will be referred to by pgn from this point
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
  def toOware: oware.PromotableRole
}

object Role {

  final case class ChessRole(r: chess.Role) extends Role {
    val forsyth = r.forsyth
    val pgn = r.pgn
    val binaryInt = r.binaryInt
    val hashInt = r.hashInt
    val name = r.name
    val groundName = r.groundName
    val storable = r.storable
    override def toString() = r.name
  }

  final case class DraughtsRole(r: draughts.Role) extends Role {
    val forsyth = r.forsyth
    val pgn = r.pdn
    val binaryInt = r.binaryInt
    val hashInt = r.hashInt
    val name = r.name
    val groundName = r.name
    val storable = false
    override def toString() = r.name
  }

  final case class FairySFRole(r: fairysf.Role) extends Role {
    val forsyth = r.forsyth
    val pgn = r.pgn
    val binaryInt = r.binaryInt
    val hashInt = r.hashInt
    val name = r.name
    val groundName = r.groundName
    val storable = r.storable
    override def toString() = r.name
  }

  final case class OwareRole(r: oware.Role) extends Role {
    val forsyth = r.forsyth
    val pgn = r.pgn
    val binaryInt = r.binaryInt
    val hashInt = r.hashInt
    val name = r.name
    val groundName = r.groundName
    val storable = r.storable
    override def toString() = r.name
  }

  final case class ChessPromotableRole(r: chess.PromotableRole) extends PromotableRole {
    val forsyth = r.forsyth
    val pgn = r.pgn
    val binaryInt = r.binaryInt
    val hashInt = r.hashInt
    val name = r.name
    val groundName = r.groundName
    val storable = r.storable
    override def toString() = r.name
    def toChess = r
    def toDraughts: draughts.PromotableRole = sys.error("Not implemented for chess")
    def toFairySF: fairysf.PromotableRole = sys.error("Not implemented for chess")
    def toOware: oware.PromotableRole = sys.error("Not implemented for oware")
  }

  final case class DraughtsPromotableRole(r: draughts.PromotableRole) extends PromotableRole {
    val forsyth = r.forsyth
    val pgn = r.pdn
    val binaryInt = r.binaryInt
    val hashInt = r.hashInt
    val name = r.name
    val groundName = r.name
    val storable = false
    override def toString() = r.name
    def toDraughts = r
    def toChess: chess.PromotableRole = sys.error("Not implemented for draughts")
    def toFairySF: fairysf.PromotableRole = sys.error("Not implemented for draughts")
    def toOware: oware.PromotableRole = sys.error("Not implemented for oware")
  }

  final case class FairySFPromotableRole(r: fairysf.PromotableRole) extends PromotableRole {
    val forsyth = r.forsyth
    val pgn = r.pgn
    val binaryInt = r.binaryInt
    val hashInt = r.hashInt
    val name = r.name
    val groundName = r.groundName
    val storable = r.storable
    override def toString() = r.name
    def toDraughts: draughts.PromotableRole = sys.error("Not implemented for fairysf")
    def toChess: chess.PromotableRole = sys.error("Not implemented for fairysf")
    def toFairySF = r
    def toOware: oware.PromotableRole = sys.error("Not implemented for oware")
  }

  final case class OwarePromotableRole(r: oware.PromotableRole) extends PromotableRole {
    val forsyth = r.forsyth
    val pgn = r.pgn
    val binaryInt = r.binaryInt
    val hashInt = r.hashInt
    val name = r.name
    val groundName = r.groundName
    val storable = r.storable
    override def toString() = r.name
    def toDraughts: draughts.PromotableRole = sys.error("Not implemented for oware")
    def toChess: chess.PromotableRole = sys.error("Not implemented for oware")
    def toFairySF = fairy.PromotableRole = sys.error("Not implemented for oware")
    def toOware: r
  }

  //lila
  def all(lib: GameLogic): List[Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.all.map(DraughtsRole)
    case GameLogic.Chess()    => chess.Role.all.map(ChessRole)
    case GameLogic.FairySF()  => fairysf.Role.all.map(FairySFRole)
    case GameLogic.Oware()    => oware.Role.all.map(OwareRole)
  }

  def allPromotable(lib: GameLogic): List[PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.allPromotable.map(DraughtsPromotableRole)
    case GameLogic.Chess()    => chess.Role.allPromotable.map(ChessPromotableRole)
    case GameLogic.FairySF()  => fairysf.Role.allPromotable.map(FairySFPromotableRole)
    case GameLogic.Oware()    => oware.Role.allPromotable.map(OwarePromotableRole)
  }

  def allByForsyth(lib: GameLogic): Map[Char, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByForsyth.map{case(f, r) => (f, DraughtsRole(r))}
    case GameLogic.Chess()    => chess.Role.allByForsyth.map{case(f, r) => (f, ChessRole(r))}
    case GameLogic.FairySF()  => fairysf.Role.allByForsyth.map{case(f, r) => (f, FairySFRole(r))}
    case GameLogic.Oware()    => oware.Role.allByForsyth.map{case(f, r) => (f, OwareRole(r))}
  }

  def allByForsyth(lib: GameLogic, gf: GameFamily): Map[Char, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByForsyth.map{case(f, r) => (f, DraughtsRole(r))}
    case GameLogic.Chess() => chess.Role.allByForsyth.map{case(f, r) => (f, ChessRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allByForsyth(gf).map{case(f, r) => (f, FairySFRole(r))}
    case GameLogic.Oware() => oware.Role.allByForsyth(gf).map{case(f, r) => (f, OwareRole(r))}
  }

  def allByPgn(lib: GameLogic): Map[Char, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByPdn.map{case(p, r) => (p, DraughtsRole(r))}
    case GameLogic.Chess() => chess.Role.allByPgn.map{case(p, r) => (p, ChessRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allByPgn.map{case(p, r) => (p, FairySFRole(r))}
    case GameLogic.Oware() => oware.Role.allByPgn.map{case(p, r) => (p, OwareRole(r))}
  }

  def allByPgn(lib: GameLogic, gf: GameFamily): Map[Char, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByPdn.map{case(p, r) => (p, DraughtsRole(r))}
    case GameLogic.Chess() => chess.Role.allByPgn.map{case(p, r) => (p, ChessRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allByPgn(gf).map{case(p, r) => (p, FairySFRole(r))}
    case GameLogic.Oware() => oware.Role.allByPgn(gf).map{case(p, r) => (p, OwareRole(r))}
  }

  def allByName(lib: GameLogic): Map[String, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByName.map{case(n, r) => (n, DraughtsRole(r))}
    case GameLogic.Chess() => chess.Role.allByName.map{case(n, r) => (n, ChessRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allByName.map{case(n, r) => (n, FairySFRole(r))}
    case GameLogic.Oware() => oware.Role.allByName.map{case(n, r) => (n, OwareRole(r))}
  }

  def allByName(lib: GameLogic, gf: GameFamily): Map[String, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByName.map{case(n, r) => (n, DraughtsRole(r))}
    case GameLogic.Chess() => chess.Role.allByName.map{case(n, r) => (n, ChessRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allByName(gf).map{case(n, r) => (n, FairySFRole(r))}
    case GameLogic.Oware() => oware.Role.allByName(gf).map{case(n, r) => (n, OwareRole(r))}
  }

  def allByGroundName(lib: GameLogic): Map[String, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByName.map{case(n, r) => (n, DraughtsRole(r))}
    case GameLogic.Chess() => chess.Role.allByGroundName.map{case(n, r) => (n, ChessRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allByGroundName.map{case(n, r) => (n, FairySFRole(r))}
    case GameLogic.Oware() => oware.Role.allByGroundName.map{case(n, r) => (n, OwareRole(r))}
  }

  def allByGroundName(lib: GameLogic, gf: GameFamily): Map[String, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByName.map{case(n, r) => (n, DraughtsRole(r))}
    case GameLogic.Chess() => chess.Role.allByGroundName.map{case(n, r) => (n, ChessRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allByGroundName(gf).map{case(n, r) => (n, FairySFRole(r))}
    case GameLogic.Oware() => oware.Role.allByGroundName(gf).map{case(n, r) => (n, OwareRole(r))}
  }

  def allPromotableByName(lib: GameLogic): Map[String, PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.allPromotableByName.map{case(n, r) => (n, DraughtsPromotableRole(r))}
    case GameLogic.Chess() => chess.Role.allPromotableByName.map{case(n, r) => (n, ChessPromotableRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allPromotableByName.map{case(n, r) => (n, FairySFPromotableRole(r))}
    case GameLogic.Oware() => oware.Role.allPromotableByName.map{case(n, r) => (n, OwarePromotableRole(r))}
  }

  def allPromotableByName(lib: GameLogic, gf: GameFamily): Map[String, PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.allPromotableByName.map{case(n, r) => (n, DraughtsPromotableRole(r))}
    case GameLogic.Chess() => chess.Role.allPromotableByName.map{case(n, r) => (n, ChessPromotableRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allPromotableByName(gf).map{case(n, r) => (n, FairySFPromotableRole(r))}
    case GameLogic.Oware() => oware.Role.allPromotableByName(gf).map{case(n, r) => (n, OwarePromotableRole(r))}
  }

  def allPromotableByForsyth(lib: GameLogic): Map[Char, PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.allPromotableByForsyth.map{case(f, r) => (f, DraughtsPromotableRole(r))}
    case GameLogic.Chess() => chess.Role.allPromotableByForsyth.map{case(f, r) => (f, ChessPromotableRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allPromotableByForsyth.map{case(f, r) => (f, FairySFPromotableRole(r))}
    case GameLogic.Oware() => oware.Role.allPromotableByForsyth.map{case(f, r) => (f, OwarePromotableRole(r))}
  }

  def allPromotableByForsyth(lib: GameLogic, gf: GameFamily): Map[Char, PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.allPromotableByForsyth.map{case(f, r) => (f, DraughtsPromotableRole(r))}
    case GameLogic.Chess() => chess.Role.allPromotableByForsyth.map{case(f, r) => (f, ChessPromotableRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allPromotableByForsyth(gf).map{case(f, r) => (f, FairySFPromotableRole(r))}
    case GameLogic.Oware() => oware.Role.allPromotableByForsyth(gf).map{case(f, r) => (f, OwarePromotableRole(r))}
  }

  def allPromotableByPgn(lib: GameLogic): Map[Char, PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.allPromotableByPdn.map{case(p, r) => (p, DraughtsPromotableRole(r))}
    case GameLogic.Chess() => chess.Role.allPromotableByPgn.map{case(p, r) => (p, ChessPromotableRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allPromotableByPgn.map{case(p, r) => (p, FairySFPromotableRole(r))}
    case GameLogic.Oware() => oware.Role.allPromotableByPgn.map{case(p, r) => (p, OwarePromotableRole(r))}
  }

  def allPromotableByPgn(lib: GameLogic, gf: GameFamily): Map[Char, PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.allPromotableByPdn.map{case(p, r) => (p, DraughtsPromotableRole(r))}
    case GameLogic.Chess()    => chess.Role.allPromotableByPgn.map{case(p, r) => (p, ChessPromotableRole(r))}
    case GameLogic.FairySF()  => fairysf.Role.allPromotableByPgn(gf).map{case(p, r) => (p, FairySFPromotableRole(r))}
    case GameLogic.Oware()    => oware.Role.allPromotableByPgn(gf).map{case(p, r) => (p, OwarePromotableRole(r))}
  }

  def forsyth(lib: GameLogic, c: Char): Option[Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.forsyth(c).map(DraughtsRole)
    case GameLogic.Chess()    => chess.Role.forsyth(c).map(ChessRole)
    case GameLogic.FairySF()  => fairysf.Role.forsyth(c).map(FairySFRole)
    case GameLogic.Oware()    => oware.Role.forsyth(c).map(OwareRole)
  }

  def promotable(lib: GameLogic, gf: GameFamily, c: Char): Option[PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.promotable(c).map(DraughtsPromotableRole)
    case GameLogic.Chess()    => chess.Role.promotable(c).map(ChessPromotableRole)
    case GameLogic.FairySF()  => fairysf.Role.promotable(gf, c).map(FairySFPromotableRole)
    case GameLogic.Oware()    => oware.Role.promotable(gf, c).map(OwarePromotableRole)
  }

  def promotable(lib: GameLogic, gf: GameFamily, name: String): Option[PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.promotable(name).map(DraughtsPromotableRole)
    case GameLogic.Chess()    => chess.Role.promotable(name).map(ChessPromotableRole)
    case GameLogic.FairySF()  => fairysf.Role.promotable(gf, name).map(FairySFPromotableRole)
    case GameLogic.Oware()    => oware.Role.promotable(gf, name).map(OwarePromotableRole)
  }

  def promotable(lib: GameLogic, gf: GameFamily, name: Option[String]): Option[PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.promotable(name).map(DraughtsPromotableRole)
    case GameLogic.Chess()    => chess.Role.promotable(name).map(ChessPromotableRole)
    case GameLogic.FairySF()  => fairysf.Role.promotable(gf, name).map(FairySFPromotableRole)
    case GameLogic.Oware()    => oware.Role.promotable(gf, name).map(OwarePromotableRole)
  }

  def storable(lib: GameLogic): List[Role] = lib match {
    case GameLogic.Draughts() => List()
    case GameLogic.Chess()    => chess.Role.storable.map(ChessRole)
    case GameLogic.FairySF()  => fairysf.Role.storable.map(FairySFRole)
    case GameLogic.Oware()    => oware.Role.storable.map(OwareRole)
  }

  def pgnMoveToRole(lib: GameLogic, gf: GameFamily, c: Char): Role = lib match {
    case GameLogic.Draughts() => DraughtsRole(draughts.Role.pdnMoveToRole(c))
    case GameLogic.Chess()    => ChessRole(chess.Role.pgnMoveToRole(c))
    case GameLogic.FairySF()  => FairySFRole(fairysf.Role.pgnMoveToRole(gf, c))
    case GameLogic.Oware()    => OwareRole(oware.Role.pgnMoveToRole(gf, c))
  }

  def javaSymbolToRole(lib: GameLogic, s: String): Role = lib match {
    case GameLogic.Draughts() => DraughtsRole(draughts.Role.javaSymbolToRole(s))
    case GameLogic.Chess()    => ChessRole(chess.Role.javaSymbolToRole(s))
    case GameLogic.FairySF()  => FairySFRole(fairysf.Role.javaSymbolToRole(s))
    case GameLogic.Oware()    => OwareRole(oware.Role.javaSymbolToRole(s))
  }

  def wrap(pr: chess.PromotableRole): PromotableRole = ChessPromotableRole(pr)
  def wrap(pr: draughts.PromotableRole): PromotableRole = DraughtsPromotableRole(pr)
  def wrap(pr: fairysf.PromotableRole): PromotableRole = FairySFPromotableRole(pr)
  def wrap(pr: oware.PromotableRole): PromotableRole = OwarePromotableRole(pr)

}
