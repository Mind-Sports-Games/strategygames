package strategygames

sealed trait Role {
  val forsyth: Char
  //draughts.Role.pdn will be referred to by pgn from this point
  val pgn: Char
  val name: String
  val groundName: String
  val binaryInt: Int
  def toString(): String
}

sealed trait PromotableRole extends Role {
  // TODO: These functions lie, but we'll have to get over it until
  //       we find the right pattern
  def toChess: chess.PromotableRole
  def toDraughts: draughts.PromotableRole
}

object Role {

  final case class ChessRole(r: chess.Role) extends Role {
    val forsyth = r.forsyth
    val pgn = r.pgn
    val binaryInt = r.binaryInt
    val name = r.name
    val groundName = r.groundName
    override def toString() = r.name
  }

  final case class DraughtsRole(r: draughts.Role) extends Role {
    val forsyth = r.forsyth
    val pgn = r.pdn
    val binaryInt = r.binaryInt
    val name = r.name
    val groundName = r.name
    override def toString() = r.name
  }

  final case class ChessPromotableRole(r: chess.PromotableRole) extends PromotableRole {
    val forsyth = r.forsyth
    val pgn = r.pgn
    val binaryInt = r.binaryInt
    val name = r.name
    val groundName = r.groundName
    override def toString() = r.name
    def toChess = r
    def toDraughts: draughts.PromotableRole = sys.error("Not implemented for chess")
  }

  final case class DraughtsPromotableRole(r: draughts.PromotableRole) extends PromotableRole {
    val forsyth = r.forsyth
    val pgn = r.pdn
    val binaryInt = r.binaryInt
    val name = r.name
    val groundName = r.name
    override def toString() = r.name
    def toDraughts = r
    def toChess: chess.PromotableRole = sys.error("Not implemented for draughts")
  }

  def all(lib: GameLogic): List[Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.all.map(DraughtsRole)
    case GameLogic.Chess()    => chess.Role.all.map(ChessRole)
  }

  def allPromotable(lib: GameLogic): List[PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.allPromotable.map(DraughtsPromotableRole)
    case GameLogic.Chess()    => chess.Role.allPromotable.map(ChessPromotableRole)
  }

  def allByForsyth(lib: GameLogic): Map[Char, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByForsyth.map{case(f, r) => (f, DraughtsRole(r))}
    case GameLogic.Chess() => chess.Role.allByForsyth.map{case(f, r) => (f, ChessRole(r))}
  }

  def allByPgn(lib: GameLogic): Map[Char, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByPdn.map{case(p, r) => (p, DraughtsRole(r))}
    case GameLogic.Chess() => chess.Role.allByPgn.map{case(p, r) => (p, ChessRole(r))}
  }

  def allByName(lib: GameLogic): Map[String, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByName.map{case(n, r) => (n, DraughtsRole(r))}
    case GameLogic.Chess() => chess.Role.allByName.map{case(n, r) => (n, ChessRole(r))}
  }

  def allByGroundName(lib: GameLogic): Map[String, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByName.map{case(n, r) => (n, DraughtsRole(r))}
    case GameLogic.Chess() => chess.Role.allByGroundName.map{case(n, r) => (n, ChessRole(r))}
  }

  def allByBinaryInt(lib: GameLogic): Map[Int, Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.allByBinaryInt.map{case(n, r) => (n, DraughtsRole(r))}
    case GameLogic.Chess() => chess.Role.allByBinaryInt.map{case(n, r) => (n, ChessRole(r))}
  }

  def allPromotableByName(lib: GameLogic): Map[String, PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.allPromotableByName.map{case(n, r) => (n, DraughtsPromotableRole(r))}
    case GameLogic.Chess() => chess.Role.allPromotableByName.map{case(n, r) => (n, ChessPromotableRole(r))}
  }

  def allPromotableByForsyth(lib: GameLogic): Map[Char, PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.allPromotableByForsyth.map{case(f, r) => (f, DraughtsPromotableRole(r))}
    case GameLogic.Chess() => chess.Role.allPromotableByForsyth.map{case(f, r) => (f, ChessPromotableRole(r))}
  }

  def allPromotableByPgn(lib: GameLogic): Map[Char, PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.allPromotableByPdn.map{case(p, r) => (p, DraughtsPromotableRole(r))}
    case GameLogic.Chess() => chess.Role.allPromotableByPgn.map{case(p, r) => (p, ChessPromotableRole(r))}
  }

  def forsyth(lib: GameLogic, c: Char): Option[Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.forsyth(c).map(DraughtsRole)
    case GameLogic.Chess()    => chess.Role.forsyth(c).map(ChessRole)
  }

  def binaryInt(lib: GameLogic, i: Int): Option[Role] = lib match {
    case GameLogic.Draughts() => draughts.Role.binaryInt(i).map(DraughtsRole)
    case GameLogic.Chess()    => chess.Role.binaryInt(i).map(ChessRole)
  }

  def promotable(lib: GameLogic, c: Char): Option[PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.promotable(c).map(DraughtsPromotableRole)
    case GameLogic.Chess()    => chess.Role.promotable(c).map(ChessPromotableRole)
  }

  def promotable(lib: GameLogic, name: String): Option[PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.promotable(name).map(DraughtsPromotableRole)
    case GameLogic.Chess()    => chess.Role.promotable(name).map(ChessPromotableRole)
  }

  def promotable(lib: GameLogic, name: Option[String]): Option[PromotableRole] = lib match {
    case GameLogic.Draughts() => draughts.Role.promotable(name).map(DraughtsPromotableRole)
    case GameLogic.Chess()    => chess.Role.promotable(name).map(ChessPromotableRole)
  }

  def pgnMoveToRole(lib: GameLogic, c: Char): Role = lib match {
    case GameLogic.Draughts() => DraughtsRole(draughts.Role.pdnMoveToRole(c))
    case GameLogic.Chess()    => ChessRole(chess.Role.pgnMoveToRole(c))
  }

  def javaSymbolToRole(lib: GameLogic, s: String): Role = lib match {
    case GameLogic.Draughts() => DraughtsRole(draughts.Role.javaSymbolToRole(s))
    case GameLogic.Chess()    => ChessRole(chess.Role.javaSymbolToRole(s))
  }

  def wrap(pr: chess.PromotableRole): PromotableRole = ChessPromotableRole(pr)
  def wrap(pr: draughts.PromotableRole): PromotableRole = DraughtsPromotableRole(pr)

}
