package strategygames.chess

import strategygames.{ P1, P2, Player }

sealed trait Role {
  val forsyth: Char
  lazy val forsythUpper: Char = forsyth.toUpper
  lazy val pgn: Char          = forsythUpper
  lazy val name               = toString.toLowerCase
  lazy val groundName         = s"${forsyth}-piece"
  val projection: Boolean
  val binaryInt: Int
  val hashInt: Int
  val storable: Boolean
  val dirs: Directions
  def dir(from: Pos, to: Pos): Option[Direction]
  final def -(player: Player) = Piece(player, this)
  final def p1                = this - P1
  final def p2                = this - P2
}
sealed trait PromotableRole extends Role

/** Promotable in antichess.
  */
case object King extends PromotableRole {
  val forsyth                 = 'k'
  val dirs: Directions        = Queen.dirs
  def dir(from: Pos, to: Pos) = None
  val projection              = false
  val binaryInt               = 1
  val hashInt                 = 5
  val storable                = false
}

case object Queen      extends PromotableRole {
  val forsyth                 = 'q'
  val dirs: Directions        = Rook.dirs ::: Bishop.dirs
  def dir(from: Pos, to: Pos) = Rook.dir(from, to) orElse Bishop.dir(from, to)
  val projection              = true
  val binaryInt               = 2
  val hashInt                 = 4
  val storable                = true
}
case object Rook       extends PromotableRole {
  val forsyth                 = 'r'
  val dirs: Directions        = List(_.up, _.down, _.left, _.right)
  def dir(from: Pos, to: Pos) =
    if (to ?| from)
      Option(if (to ?^ from) (_.up) else (_.down))
    else if (to ?- from)
      Option(if (to ?< from) (_.left) else (_.right))
    else None
  val projection              = true
  val binaryInt               = 3
  val hashInt                 = 3
  val storable                = true
}
case object Bishop     extends PromotableRole {
  val forsyth                 = 'b'
  val dirs: Directions        = List(_.upLeft, _.upRight, _.downLeft, _.downRight)
  def dir(from: Pos, to: Pos) =
    if (to onSameDiagonal from)
      Option(if (to ?^ from) {
        if (to ?< from) (_.upLeft) else (_.upRight)
      } else {
        if (to ?< from) (_.downLeft) else (_.downRight)
      })
    else None
  val projection              = true
  val binaryInt               = 5
  val hashInt                 = 2
  val storable                = true
}
case object Knight     extends PromotableRole {
  val forsyth                 = 'n'
  val dirs: Directions        = List(
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
  val binaryInt               = 4
  val hashInt                 = 1
  val storable                = true
}
case object Pawn       extends Role           {
  val forsyth                 = 'p'
  val dirs: Directions        = Nil
  def dir(from: Pos, to: Pos) = None
  val projection              = false
  val binaryInt               = 6
  val hashInt                 = 0
  val storable                = true
}
case object LOAChecker extends Role           {
  val forsyth                 = 'l'
  val dirs: Directions        = Queen.dirs
  def dir(from: Pos, to: Pos) = Queen.dir(from, to)
  val projection              = false
  val binaryInt               = 8
  val hashInt                 = 6
  val storable                = false
}

object Role {

  val all: List[Role]                                        = List(King, Queen, Rook, Bishop, Knight, Pawn, LOAChecker)
  val allPromotable: List[PromotableRole]                    = List(Queen, Rook, Bishop, Knight, King)
  val allByForsyth: Map[Char, Role]                          = all map { r =>
    (r.forsyth, r)
  } toMap
  val allByPgn: Map[Char, Role]                              = all map { r =>
    (r.pgn, r)
  } toMap
  val allByName: Map[String, Role]                           = all map { r =>
    (r.name, r)
  } toMap
  val allByGroundName: Map[String, Role]                     = all map { r =>
    (r.groundName, r)
  } toMap
  val allByBinaryInt: Map[Int, Role]                         = all map { r =>
    (r.binaryInt, r)
  } toMap
  val allByHashInt: Map[Int, Role]                           = all map { r =>
    (r.hashInt, r)
  } toMap
  val allPromotableByName: Map[String, PromotableRole]       =
    allPromotable map { r =>
      (r.toString, r)
    } toMap
  val allPromotableByGroundName: Map[String, PromotableRole] =
    allPromotable map { r =>
      (r.groundName, r)
    } toMap
  val allPromotableByForsyth: Map[Char, PromotableRole]      =
    allPromotable map { r =>
      (r.forsyth, r)
    } toMap
  val allPromotableByPgn: Map[Char, PromotableRole]          =
    allPromotable map { r =>
      (r.pgn, r)
    } toMap

  def forsyth(c: Char): Option[Role] = allByForsyth get c

  def binaryInt(i: Int): Option[Role] = allByBinaryInt get i

  def hashInt(i: Int): Option[Role] = allByHashInt get i

  def promotable(c: Char): Option[PromotableRole] =
    allPromotableByForsyth get c

  def promotable(name: String): Option[PromotableRole] =
    allPromotableByName get name.capitalize

  def promotable(name: Option[String]): Option[PromotableRole] =
    name flatMap promotable

  def storable: List[Role] = all.filter(_.storable)

  def pgnMoveToRole(c: Char): Role =
    allByPgn.get(c) match {
      case Some(r) => r
      case None    => if (c == 'O') King else Pawn
    }

  def javaSymbolToRole(s: String): Role =
    allByPgn
      .get(
        s.headOption match {
          case Some(c) => c
          case None    => 'P' // JavaRole.PAWN.symbol is ""
        }
      )
      .get

  def valueOf(r: Role): Option[Int] =
    r match {
      case Pawn       => Option(1)
      case Knight     => Option(3)
      case Bishop     => Option(3)
      case Rook       => Option(5)
      case Queen      => Option(9)
      case King       => None
      case LOAChecker => None
    }
}
