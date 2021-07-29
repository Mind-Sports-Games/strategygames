package chess

sealed trait Role {
  val forsyth: Char
  lazy val forsythUpper: Char = forsyth.toUpper
  lazy val name = toString.toLowerCase
  val ruleSet: RuleSet
}

sealed trait PromotableRole extends Role

sealed trait ChessRole extends Role {
  lazy val ruleSet: RuleSet = Chess
  lazy val pgn: Char = forsythUpper
  val projection: Boolean
  val dirs: Directions
  def dir(from: Pos, to: Pos): Option[Direction]
}

sealed trait PromotableChessRole extends ChessRole with PromotableRole

/** Promotable in antichess.
  */
case object King extends PromotableChessRole {
  val forsyth                 = 'k'
  val dirs: Directions        = Queen.dirs
  def dir(from: Pos, to: Pos) = None
  val projection              = false
}

case object Queen extends PromotableChessRole {
  val forsyth                 = 'q'
  val dirs: Directions        = Rook.dirs ::: Bishop.dirs
  def dir(from: Pos, to: Pos) = Rook.dir(from, to) orElse Bishop.dir(from, to)
  val projection              = true
}

case object Rook extends PromotableChessRole {
  val forsyth          = 'r'
  val dirs: Directions = List(_.up, _.down, _.left, _.right)
  def dir(from: Pos, to: Pos) =
    if (to ?| from)
      Option(if (to ?^ from) (_.up) else (_.down))
    else if (to ?- from)
      Option(if (to ?< from) (_.left) else (_.right))
    else None
  val projection = true
}

case object Bishop extends PromotableChessRole {
  val forsyth          = 'b'
  val dirs: Directions = List(_.upLeft, _.upRight, _.downLeft, _.downRight)
  def dir(from: Pos, to: Pos) =
    if (to onSameDiagonal from)
      Option(if (to ?^ from) {
        if (to ?< from) (_.upLeft) else (_.upRight)
      } else {
        if (to ?< from) (_.downLeft) else (_.downRight)
      })
    else None
  val projection = true
}

case object Knight extends PromotableChessRole {
  val forsyth = 'n'
  val dirs: Directions = List(
    p => Pos.at(p.file.index - 1, p.rank.index + 2),
    p => Pos.at(p.file.index - 1, p.rank.index - 2),
    p => Pos.at(p.file.index + 1, p.rank.index + 2),
    p => Pos.at(p.file.index + 1, p.rank.index - 2),
    p => Pos.at(p.file.index - 2, p.rank.index + 1),
    p => Pos.at(p.file.index - 2, p.rank.index - 1),
    p => Pos.at(p.file.index + 2, p.rank.index + 1),
    p => Pos.at(p.file.index + 2, p.rank.index - 1)
  )
  def dir(from: Pos, to: Pos) = None
  val projection              = false
}

case object Pawn extends ChessRole {
  val forsyth                 = 'p'
  val dirs: Directions        = Nil
  def dir(from: Pos, to: Pos) = None
  val projection              = false
}

case object LOAChecker extends ChessRole {
  val forsyth                 = 'l'
  val dirs: Directions        = Queen.dirs
  def dir(from: Pos, to: Pos) = Queen.dir(from, to)
  val projection              = false
}

//Draughts pieces
sealed trait DraughtsRole extends Role {
  lazy val ruleSet: RuleSet = Draughts
  lazy val pdn: Char = forsyth
}

sealed trait PromotableDraughtsRole extends DraughtsRole with PromotableRole

case object CheckerKing extends PromotableDraughtsRole {
  val forsyth = 'K'
}

case object CheckerMan extends DraughtsRole {
  val forsyth = ' '
}

case object GhostCheckerMan extends DraughtsRole {
  val forsyth = 'G'
}

case object GhostCheckerKing extends DraughtsRole {
  val forsyth = 'P'
}

object Role {

  val all: List[Role]                     = List(King, Queen, Rook, Bishop, Knight, Pawn, LOAChecker, CheckerMan, CheckerKing)
  val allPromotable: List[PromotableRole] = List(Queen, Rook, Bishop, Knight, King, CheckerKing)

  val allChess: List[Role] = all collect {case r: ChessRole => r}
  val allDraughts: List[Role] = all collect {case r: DraughtsRole => r}
  val allChessPromotable: List[PromotableRole] = allPromotable collect {case r: ChessRole => r}
  val allDraughtsPromotable: List[PromotableRole] = allPromotable collect {case r: DraughtsRole => r}

  //val allByFamily: Map[String, List[Role]] = (all map { r =>
  //  (r, r.family)
  //} toMap) groupBy { case (_, value) => value } mapValues(_.keys.toList) toMap

  val allByForsyth: Map[Char, Role] = all map { r =>
    (r.forsyth, r)
  } toMap
  val allByPgn: Map[Char, ChessRole] = all collect {case r: ChessRole => r} map { r =>
    (r.pgn, r)
  } toMap
  val allByPdn: Map[Char, DraughtsRole] = all collect {case r: DraughtsRole => r} map { r =>
    (r.pdn, r)
  } toMap
  val allByName: Map[String, Role] = all map { r =>
    (r.name, r)
  } toMap
  val allPromotableByName: Map[String, PromotableRole] =
    allPromotable map { r =>
      (r.toString, r)
    } toMap
  val allPromotableByForsyth: Map[Char, PromotableRole] =
    allPromotable map { r =>
      (r.forsyth, r)
    } toMap
  val allPromotableByPgn: Map[Char, PromotableChessRole] =
    allPromotable collect {case r: PromotableChessRole => r} map { r =>
      (r.pgn, r)
    } toMap

  def forsyth(c: Char): Option[Role] = allByForsyth get c

  def promotable(c: Char): Option[PromotableRole] =
    allPromotableByForsyth get c

  def promotable(name: String): Option[PromotableRole] =
    allPromotableByName get name.capitalize

  def promotable(name: Option[String]): Option[PromotableRole] =
    name flatMap promotable

  def valueOf(r: Role): Option[Int] =
    r match {
      case Pawn   => Option(1)
      case Knight => Option(3)
      case Bishop => Option(3)
      case Rook   => Option(5)
      case Queen  => Option(9)
      case King   => None
      case LOAChecker  => None
      case CheckerMan  => Option(1)
      case CheckerKing => Option(2)
      case _           => None
    }
}
