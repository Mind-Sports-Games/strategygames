package strategygames

sealed trait Role {
  val forsyth: Char
  //draughts.Role.pdn will be referred to by pgn from this point
  val pgn: Char
  val name: String
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
}

object Role {

  final case class ChessRole(r: chess.Role) extends Role {
    val forsyth = r.forsyth
    val pgn = r.pgn
    val binaryInt = r.binaryInt
    val hashInt = r.hashInt
    val name = r.name
    val storable = r.storable
    override def toString() = r.name
  }

  final case class DraughtsRole(r: draughts.Role) extends Role {
    val forsyth = r.forsyth
    val pgn = r.pdn
    val binaryInt = r.binaryInt
    val hashInt = r.hashInt
    val name = r.name
    val storable = false
    override def toString() = r.name
  }

  final case class FairySFRole(r: fairysf.Role) extends Role {
    val forsyth = r.forsyth
    val pgn = r.pgn
    val binaryInt = r.binaryInt
    val hashInt = r.hashInt
    val name = r.name
    val storable = r.storable
    override def toString() = r.name
  }

  final case class ChessPromotableRole(r: chess.PromotableRole) extends PromotableRole {
    val forsyth = r.forsyth
    val pgn = r.pgn
    val binaryInt = r.binaryInt
    val hashInt = r.hashInt
    val name = r.name
    val storable = r.storable
    override def toString() = r.name
    def toChess = r
    def toDraughts: draughts.PromotableRole = sys.error("Not implemented for chess")
    def toFairySF: fairysf.PromotableRole = sys.error("Not implemented for chess")
  }

  final case class DraughtsPromotableRole(r: draughts.PromotableRole) extends PromotableRole {
    val forsyth = r.forsyth
    val pgn = r.pdn
    val binaryInt = r.binaryInt
    val hashInt = r.hashInt
    val name = r.name
    val storable = false
    override def toString() = r.name
    def toDraughts = r
    def toChess: chess.PromotableRole = sys.error("Not implemented for draughts")
    def toFairySF: fairysf.PromotableRole = sys.error("Not implemented for draughts")
  }

  final case class FairySFPromotableRole(r: fairysf.PromotableRole) extends PromotableRole {
    val forsyth = r.forsyth
    val pgn = r.pgn
    val binaryInt = r.binaryInt
    val hashInt = r.hashInt
    val name = r.name
    val storable = r.storable
    override def toString() = r.name
    def toDraughts: draughts.PromotableRole = sys.error("Not implemented for fairysf")
    def toChess: chess.PromotableRole = sys.error("Not implemented for fairysf")
    def toFairySF = r
  }

  def all(lib: GameLogic): List[Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.all.map(DraughtsRole)
    case GameLogic.Chess()    => chess.Role.all.map(ChessRole)
    case GameLogic.FairySF()  => fairysf.Role.all.map(FairySFRole)
  }

  def allPromotable(lib: GameLogic): List[PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.allPromotable.map(DraughtsPromotableRole)
    case GameLogic.Chess()    => chess.Role.allPromotable.map(ChessPromotableRole)
    case GameLogic.FairySF()  => fairysf.Role.allPromotable.map(FairySFPromotableRole)
  }

  def allByForsyth(lib: GameLogic): Map[Char, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByForsyth.map{case(f, r) => (f, DraughtsRole(r))}
    case GameLogic.Chess() => chess.Role.allByForsyth.map{case(f, r) => (f, ChessRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allByForsyth.map{case(f, r) => (f, FairySFRole(r))}
  }

  def allByPgn(lib: GameLogic): Map[Char, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByPdn.map{case(p, r) => (p, DraughtsRole(r))}
    case GameLogic.Chess() => chess.Role.allByPgn.map{case(p, r) => (p, ChessRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allByPgn.map{case(p, r) => (p, FairySFRole(r))}
  }

  def allByName(lib: GameLogic): Map[String, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByName.map{case(n, r) => (n, DraughtsRole(r))}
    case GameLogic.Chess() => chess.Role.allByName.map{case(n, r) => (n, ChessRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allByName.map{case(n, r) => (n, FairySFRole(r))}
  }

  def allByBinaryInt(lib: GameLogic): Map[Int, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByBinaryInt.map{case(n, r) => (n, DraughtsRole(r))}
    case GameLogic.Chess() => chess.Role.allByBinaryInt.map{case(n, r) => (n, ChessRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allByBinaryInt.map{case(n, r) => (n, FairySFRole(r))}
  }

  def allByHashInt(lib: GameLogic): Map[Int, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByHashInt.map{case(n, r) => (n, DraughtsRole(r))}
    case GameLogic.Chess() => chess.Role.allByHashInt.map{case(n, r) => (n, ChessRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allByHashInt.map{case(n, r) => (n, FairySFRole(r))}
  }

  def allPromotableByName(lib: GameLogic): Map[String, PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.allPromotableByName.map{case(n, r) => (n, DraughtsPromotableRole(r))}
    case GameLogic.Chess() => chess.Role.allPromotableByName.map{case(n, r) => (n, ChessPromotableRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allPromotableByName.map{case(n, r) => (n, FairySFPromotableRole(r))}
  }

  def allPromotableByForsyth(lib: GameLogic): Map[Char, PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.allPromotableByForsyth.map{case(f, r) => (f, DraughtsPromotableRole(r))}
    case GameLogic.Chess() => chess.Role.allPromotableByForsyth.map{case(f, r) => (f, ChessPromotableRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allPromotableByForsyth.map{case(f, r) => (f, FairySFPromotableRole(r))}
  }

  def allPromotableByPgn(lib: GameLogic): Map[Char, PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.allPromotableByPdn.map{case(p, r) => (p, DraughtsPromotableRole(r))}
    case GameLogic.Chess() => chess.Role.allPromotableByPgn.map{case(p, r) => (p, ChessPromotableRole(r))}
    case GameLogic.FairySF() => fairysf.Role.allPromotableByPgn.map{case(p, r) => (p, FairySFPromotableRole(r))}
  }

  def forsyth(lib: GameLogic, c: Char): Option[Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.forsyth(c).map(DraughtsRole)
    case GameLogic.Chess()    => chess.Role.forsyth(c).map(ChessRole)
    case GameLogic.FairySF()  => fairysf.Role.forsyth(c).map(FairySFRole)
  }

  def binaryInt(lib: GameLogic, i: Int): Option[Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.binaryInt(i).map(DraughtsRole)
    case GameLogic.Chess()    => chess.Role.binaryInt(i).map(ChessRole)
    case GameLogic.FairySF()  => fairysf.Role.binaryInt(i).map(FairySFRole)
  }

  def hashInt(lib: GameLogic, i: Int): Option[Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.hashInt(i).map(DraughtsRole)
    case GameLogic.Chess()    => chess.Role.hashInt(i).map(ChessRole)
    case GameLogic.FairySF()  => fairysf.Role.hashInt(i).map(FairySFRole)
  }

  def promotable(lib: GameLogic, c: Char): Option[PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.promotable(c).map(DraughtsPromotableRole)
    case GameLogic.Chess()    => chess.Role.promotable(c).map(ChessPromotableRole)
    case GameLogic.FairySF()  => fairysf.Role.promotable(c).map(FairySFPromotableRole)
  }

  def promotable(lib: GameLogic, name: String): Option[PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.promotable(name).map(DraughtsPromotableRole)
    case GameLogic.Chess()    => chess.Role.promotable(name).map(ChessPromotableRole)
    case GameLogic.FairySF()  => fairysf.Role.promotable(name).map(FairySFPromotableRole)
  }

  def promotable(lib: GameLogic, name: Option[String]): Option[PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.promotable(name).map(DraughtsPromotableRole)
    case GameLogic.Chess()    => chess.Role.promotable(name).map(ChessPromotableRole)
    case GameLogic.FairySF()  => fairysf.Role.promotable(name).map(FairySFPromotableRole)
  }

  def storable(lib: GameLogic): List[Role] = lib match {
    case GameLogic.Draughts() => List()
    case GameLogic.Chess()    => chess.Role.storable.map(ChessRole)
    case GameLogic.FairySF()  => fairysf.Role.storable.map(FairySFRole)
  }

  def pgnMoveToRole(lib: GameLogic, c: Char): Role = lib match {
    case GameLogic.Draughts() => DraughtsRole(draughts.Role.pdnMoveToRole(c))
    case GameLogic.Chess()    => ChessRole(chess.Role.pgnMoveToRole(c))
    case GameLogic.FairySF()  => FairySFRole(fairysf.Role.pgnMoveToRole(c))
  }

  def javaSymbolToRole(lib: GameLogic, s: String): Role = lib match {
    case GameLogic.Draughts() => DraughtsRole(draughts.Role.javaSymbolToRole(s))
    case GameLogic.Chess()    => ChessRole(chess.Role.javaSymbolToRole(s))
    case GameLogic.FairySF()  => FairySFRole(fairysf.Role.javaSymbolToRole(s))
  }

  def wrap(pr: chess.PromotableRole): PromotableRole = ChessPromotableRole(pr)
  def wrap(pr: draughts.PromotableRole): PromotableRole = DraughtsPromotableRole(pr)
  def wrap(pr: fairysf.PromotableRole): PromotableRole = FairySFPromotableRole(pr)

}
