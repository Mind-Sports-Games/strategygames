package strategygames

sealed trait Role {
  val forsyth: Char
  //draughts.Role.pdn will be referred to by pgn from this point
  val pgn: Char
  lazy val name = toString.toLowerCase
}

sealed trait PromotableRole extends Role

object Role {

  final case class ChessRole(r: chess.Role) extends Role {
    val forsyth = r.forsyth
    val pgn = r.pgn
  }

  final case class DraughtsRole(r: draughts.Role) extends Role {
    val forsyth = r.forsyth
    val pgn = r.pdn
  }

  final case class ChessPromotableRole(r: chess.PromotableRole) extends PromotableRole {
    val forsyth = r.forsyth
    val pgn = r.pgn
  }

  final case class DraughtsPromotableRole(r: draughts.PromotableRole) extends PromotableRole {
    val forsyth = r.forsyth
    val pgn = r.pdn
  }

  def all(lib: GameLib): List[Role] = lib match {
    case GameLib.Draughts() => draughts.Role.all.map(DraughtsRole)
    case GameLib.Chess()    => chess.Role.all.map(ChessRole)
  }

  def allPromotable(lib: GameLib): List[PromotableRole] = lib match {
    case GameLib.Draughts() => draughts.Role.allPromotable.map(DraughtsPromotableRole)
    case GameLib.Chess()    => chess.Role.allPromotable.map(ChessPromotableRole)
  }

  def allByForsyth(lib: GameLib): Map[Char, Role] = lib match {
    case GameLib.Draughts() => draughts.Role.allByForsyth.map{case(f, r) => (f, DraughtsRole(r))}
    case GameLib.Chess() => chess.Role.allByForsyth.map{case(f, r) => (f, ChessRole(r))}
  }

  def allByPgn(lib: GameLib): Map[Char, Role] = lib match {
    case GameLib.Draughts() => draughts.Role.allByPdn.map{case(p, r) => (p, DraughtsRole(r))}
    case GameLib.Chess() => chess.Role.allByPgn.map{case(p, r) => (p, ChessRole(r))}
  }

  def allByName(lib: GameLib): Map[String, Role] = lib match {
    case GameLib.Draughts() => draughts.Role.allByName.map{case(n, r) => (n, DraughtsRole(r))}
    case GameLib.Chess() => chess.Role.allByName.map{case(n, r) => (n, ChessRole(r))}
  }

  def allPromotableByName(lib: GameLib): Map[String, PromotableRole] = lib match {
    case GameLib.Draughts() => draughts.Role.allPromotableByName.map{case(n, r) => (n, DraughtsPromotableRole(r))}
    case GameLib.Chess() => chess.Role.allPromotableByName.map{case(n, r) => (n, ChessPromotableRole(r))}
  }

  def allPromotableByForsyth(lib: GameLib): Map[Char, PromotableRole] = lib match {
    case GameLib.Draughts() => draughts.Role.allPromotableByForsyth.map{case(f, r) => (f, DraughtsPromotableRole(r))}
    case GameLib.Chess() => chess.Role.allPromotableByForsyth.map{case(f, r) => (f, ChessPromotableRole(r))}
  }

  def allPromotableByPgn(lib: GameLib): Map[Char, PromotableRole] = lib match {
    case GameLib.Draughts() => draughts.Role.allPromotableByPdn.map{case(p, r) => (p, DraughtsPromotableRole(r))}
    case GameLib.Chess() => chess.Role.allPromotableByPgn.map{case(p, r) => (p, ChessPromotableRole(r))}
  }

  def forsyth(lib: GameLib, c: Char): Option[Role] = lib match {
    case GameLib.Draughts() => draughts.Role.forsyth(c).map(DraughtsRole)
    case GameLib.Chess()    => chess.Role.forsyth(c).map(ChessRole)
  }

  def promotable(lib: GameLib, c: Char): Option[PromotableRole] = lib match {
    case GameLib.Draughts() => draughts.Role.promotable(c).map(DraughtsPromotableRole)
    case GameLib.Chess()    => chess.Role.promotable(c).map(ChessPromotableRole)
  }

  def promotable(lib: GameLib, name: String): Option[PromotableRole] = lib match {
    case GameLib.Draughts() => draughts.Role.promotable(name).map(DraughtsPromotableRole)
    case GameLib.Chess()    => chess.Role.promotable(name).map(ChessPromotableRole)
  }

  def promotable(lib: GameLib, name: Option[String]): Option[PromotableRole] = lib match {
    case GameLib.Draughts() => draughts.Role.promotable(name).map(DraughtsPromotableRole)
    case GameLib.Chess()    => chess.Role.promotable(name).map(ChessPromotableRole)
  }

  /* I'm unsure how we map strategygames.Role back to strategygames.chess.Role?
  def valueOf(lib: GameLib, r: Role): Option[Int] = lib match {
    case GameLib.Draughts() => draughts.Role.valueOf(r) 
    case GameLib.Chess()    => chess.Role.valueOf(r)
  }
  */
}
